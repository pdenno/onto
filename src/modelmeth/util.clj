(ns modelmeth.util
  (:require [clojure.pprint :refer (cl-format pprint)]
            [clojure.walk :refer-only (prewalk prewalk-demo)]
            [tawny.owl    :as owl]
            [tawny.query  :as query]
            [tawny.render]
            [tawny.lookup :as look]
            [tawny.read   :as rowl])
  (:use clojure.java.io)
  (:import
   (org.semanticweb.owlapi.model
    HasIRI
    OWLAxiom
    OWLEntity
    OWLObject
    OWLOntologyManager OWLOntology IRI
    OWLClassExpression OWLClass OWLAnnotation
    OWLDataProperty OWLObjectProperty
    OWLDataPropertyExpression ; not useful?
    OWLIndividual OWLDatatype
    OWLObjectPropertyExpression
    OWLNamedObject OWLOntologyID)
   [org.semanticweb.owlapi.search EntitySearcher]))

(defn aliases [] ; POD temporary
  (alias 'core  'modelmeth.core)
  (alias 'latex 'modelmeth.latex))

;;; Meaning of some variables use in functions:
;;;  obj - a OWLAPI object, one of OWLObject, OWLDataProperty, OWLObjectProperty
;;;  tmap - a hash-map of information collected from obj through the thing-map function.
;;; N.B. There is a single ontology-manager used by Tawny. Tawny users don't need to create one. 

(def ^:private diag (atom nil))

(def +params+ (atom {:section-offset 1}))
                     

(def tawny-types [:tawny.owl/class :tawny.owl/individual :tawny.owl/property
                  :tawny.owl/object-property :tawny.owl/data-property])

;;; POD I expected this to be http://modelmeth.nist.gov/modeling#clojureCodeNote
(def ^:const code-iri "Used to identify clojure notes from thing-mapped objects."
  (list :iri "http://modelmeth.nist.gov/modeling#clojureCode"))

(defn read-onto
  [onto iri]
  (rowl/read :iri iri
             :namespace (create-ns (:onto-namespace @+params+)) 
             :location (-> onto :location clojure.java.io/file)))

(defn simplify-tawny-annotations
  "Some are (:comment <pairs>). Some are (:annotation code-iri <pairs>)"
  [tawny-notes]
  (vec
   (doall
    (map #(let [original %]
            (as-> original ?note
              (cond (= (first ?note) :comment) (second ?note),
                    (= (first ?note) :annotation ) (-> ?note rest rest first))
              (apply hash-map ?note)
              (assoc ?note :otype (first original))))
         tawny-notes))))

(defn short-name
  "Argument is an OWLClassImpl etc."
  [obj]
  (->> obj
       look/named-entity-as-string
       (re-matches #".*\#(.*)")
       second))

(defn properties-of
  "Return a seq of properties that are relevant to the argument tmap."
  [tmap property-map]
  (let [sname (:short-name tmap)]
     (filter (fn [pm] (or (some #(= sname %) (:domains pm))
                          (some #(= sname %) (:ranges  pm))))
             (vals property-map))))

(defn thing-map
  "Return a map of information about the class. See also query/into-map-with"
  ([obj] (thing-map obj {})) ; Used by ignore? 
  ([obj property-map]
   (when (instance? OWLClass obj)
     (let [sname (short-name obj)]
       (as-> (apply hash-map (tawny.render/as-form obj :keyword true)) ?map
         (assoc ?map :short-name sname) ; POD (or label)
         (assoc ?map :var (intern (:onto-namespace @+params+) (symbol sname)))
         (assoc ?map :notes (simplify-tawny-annotations (:annotation ?map)))
         (assoc ?map :subclass-of (doall (map short-name ; POD there are other ways. See notes 2017-07-22. 
                                              (filter #(instance? OWLClass %)
                                                      (owl/direct-superclasses obj)))))
         (assoc ?map :properties (properties-of ?map property-map)))))))

(defn clojure-code
  "Return any http://modelmeth.nist.gov/modeling#clojureCode annotation"
  [obj]
  (some #(when (and (= (:otype %) :annotation)
                    (= (:type %) code-iri))
           (:literal %))
        (-> obj thing-map :notes)))

;;; POD needs to return true if a supertype is ignored. 
(defn ignore?
  "Returns true if the tawny thing has a clojure {:priority :ignore}"
  [obj]
  (if (some #(= (owl/guess-type obj) %) tawny-types)
    (when-let [code (clojure-code obj)]
      (= :ignore (:priority (read-string code))))
    true))

(defn onto-parent-child-map
  "Define the parent/child relationship as a map."
  [root]
  (let [obj2var-map
        (let [ks (remove #(ignore? (var-get %))
                         (vals (ns-interns (:onto-namespace @+params+))))
              vs (map var-get ks)]
          (clojure.set/map-invert (zipmap ks vs))),
        m (reduce (fn [index node]
                    (assoc index (get obj2var-map node)
                           (map #(get obj2var-map %)
                                (owl/direct-subclasses node))))
                  {}
                  (conj (owl/subclasses root) root))]
    (as-> m ?map
      (dissoc ?map nil)
      (reduce (fn [m k] (update m k #(vec (filter identity %))))
              ?map
              (keys ?map)))))

(defn next-paths
  "Return all paths one-step further than the argument, if any."
  [path index]
  (if (empty? path)
    []
    (vec (map #(conj path %) (vec (get index (last path)))))))

(defn onto-root-map
  "Define the ontology root structure as a nested map."
  [accum paths index]
  (if (empty? paths)
    accum
    (recur
     (assoc-in accum (first paths) {})
     (let [nexts (next-paths (first paths) index)]
       (if (empty? nexts)
         (vec (next paths))
         (into nexts (vec (next paths)))))
     index)))

(defn vectify
  "Turn a nested map like that from onto-root-map into a nested vector
   where every var leaf is followed by a vector representing its subclasses 
   (could be empty)."
  [nested-map]
  (clojure.walk/prewalk
   #(if (var? %)
      %
      (vec (interleave (keys %) (vals %))))
   nested-map))

(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint *1)))

(defn ppprint [arg]
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint arg)))
