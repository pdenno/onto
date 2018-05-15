(ns modelmeth.latex
  "Latex rendering" 
  (:require [clojure.pprint :refer (cl-format pprint)]
            [modelmeth.util :as util :refer (ppprint ppp with-onto)]
            [clojure.zip :as zip]
            [tawny.owl :as owl]
            [tawny.query :as query]
            [tawny.render]
            [tawny.lookup :as look]
            [tawny.read :as rowl])
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
  (alias 'core  'modelmeth.core))

(def ^:private diag (atom nil))
                        
(defn zip-depth
  "Return the depth of the location."
  [loc]
  (loop [loc loc
         depth 0]
    (if (not (zip/up loc))
      depth
      (recur (zip/up loc) (inc depth)))))

;;; I expand the tree with leaf-node maps before navigation and printing
;;; because this is the place to calculate :depth.
(defn latex-leaf-nodes
  "Return the onto vector structure with thing-maps replacing vars."
  [onto-vec property-map]
  (loop [loc (-> onto-vec zip/vector-zip zip/down)]
    (as-> loc ?loc
      (if (var? (zip/node ?loc))
        (zip/edit ?loc #(-> (util/thing-map % property-map) ; POD why dec below?
                            (assoc :depth (dec (+ (:section-offset @util/+params+)
                                                  (zip-depth ?loc))))))
        ?loc)
      (zip/next ?loc)
      (if (zip/end? ?loc)
        (zip/root ?loc)
        (recur ?loc)))))

(defn subsub [n]
  (cond 
    (= n 0) "\\section"
    (= n 1) "\\subsection"
    (= n 2) "\\subsubsection"
    (= n 3) "\\paragraph"
    (= n 4) "\\subparagraph"
    :else ""))

(defn entity-comment [tmap]
  (some #(when (= (:otype %) :comment) (:literal %))
        (:notes tmap)))

;;; POD Needs work. Doesn't yet describe disjoint / disjoint exhaustive.
(defn write-subclasses
  "Write text describing subtyping relationships."
  [tmap]
  (let [sname (:short-name tmap)
        stypes (sort (:subclass-of tmap))]
    (doall
     (map #(println (cl-format nil "\\\\$ ~A \\sqsubseteq ~A$" %1 %2))
          (repeat (count stypes) sname)
          stypes))))

;;; POD Should this just look at direct properties? 
(defn write-properties
  "Write text describing properties the class is involved in."
  [tmap]
  (let [sname (:short-name tmap)]
    (doall
     (map (fn [rel]
            (if (some #(= sname %) (:domains rel))
              (println (cl-format nil "\\\\$\\: ~A\\: \\textbf{~A}\\: ~{~A~^\\: \\vee~}\\: ~A$"
                                  sname
                                  (util/short-name (:obj rel))
                                  (:ranges rel)
                                  (:characteristics rel)))
              (println (cl-format nil "\\\\$\\: ~{~A~^\\: \\vee~}\\: \\textbf{~A}\\: ~A\\:  ~A$"
                                  (:domains rel)
                                  (util/short-name (:obj rel))
                                  sname
                                  (:characteristics rel)))))
          (sort #(compare (->> %1 :var meta :name)
                          (->> %2 :var meta :name))
                (:properties tmap))))
    #_(println "\\\\")))

(defn write-concept
  "Write text about concept."
  [tmap]
  (let [sname (:short-name tmap)
        comment (entity-comment tmap)]
    (println (cl-format nil "\\\\\\\\   \\textbf{~A}\\\\Description: ~A\\\\"
                        sname (or comment "+++Needs Definition+++")))
    (write-subclasses tmap)
    (write-properties tmap)))

(defn write-section-heading
  [section]
  (println (cl-format nil "~A{~A}\\\\" (subsub (:depth section)) (:short-name section)))
  (println (cl-format nil "Description: ~A\\\\" (or (entity-comment section) "+++Needs Definition+++")))
  (write-subclasses section)
  (write-properties section))

;;; POD Doesn't clojure have something like this?
(defn by-n
  "Return the argument chopped up into n-sized sub-seqs"
  [n a-seq]
  (loop [s a-seq
         result []]
    (if (empty? s) 
      result
      (recur (drop n s) (conj result (take n s))))))

(defn partition-concepts
  "Argument is a loc that names a section. Return a map f associated nodes (thing-maps)
   including the :section tmap, :direct-concepts thing-maps and :subsections thing-maps."
  [loc]
  (let [children (by-n 2 (-> loc zip/next zip/node))]
    {:section (-> loc zip/node)
     :direct-concepts (map first (filter #(-> % second empty?) children))
     :subsections     (map vec   (remove #(-> % second empty?) children))}))

(defn write-section!
  "Write a \\section, \\subsection etc. Calls itself recursively on zip/children."
  [loc]
  (cond (empty? loc) nil,
        (map? loc) (throw (ex-info "map?" {}))
        (var? loc) (throw (ex-info "var?" {}))
        (zip/end? loc) nil,
        ;;(-> loc zip/next zip/branch?)
        (-> loc zip/node map?)
        (let [parts (partition-concepts loc)
              section (:section parts)
              concepts (:direct-concepts parts)
              subsections (:subsections parts)]
          (when section (write-section-heading section))
          (doall (map write-concept concepts))
          (doall (map #(write-section! (-> % zip/vector-zip zip/down)) subsections)))))

(defn latex-write-onto-nodes!
  "Write latex for the onto-root structure"
  [v property-map]
  (-> v
      (latex-leaf-nodes property-map)
      zip/vector-zip
      zip/down
      write-section!))

(defn property-characteristics
  "Return a vector containing keywords indicating which of the 7 OWL property
   characteristics are true for the argument property."
  [p ont]
  (if (instance? OWLDataProperty p)
    (cond-> []
      (EntitySearcher/isFunctional p ont)        (conj :functional))
    (cond-> []
      (EntitySearcher/isTransitive p ont)        (conj :transitive)
      (EntitySearcher/isFunctional p ont)        (conj :functional)
      (EntitySearcher/isInverseFunctional p ont) (conj :inverse-functional)
      (EntitySearcher/isSymmetric p ont)         (conj :symmetric)
      (EntitySearcher/isAsymmetric p ont)        (conj :asymmetric)
      (EntitySearcher/isIrreflexive p ont)       (conj :irreflexive)
      (EntitySearcher/isReflexive p ont)         (conj :reflexive))))

(defn property-analysis
  "Return a map indexed by tawny vars. Entries are maps describing onto properties."
  []
  (let [o (owl/get-current-ontology)
        prop-vars
        (filter #(or (instance? OWLDataProperty (var-get %))
                     (instance? OWLObjectProperty (var-get %)))
                (vals (ns-interns *ns*)))]
    (reduce (fn [prop-map var] ; Key var already exists. I use sname to find it. 
              (let [obj (var-get var)]
                (-> prop-map
                    (assoc var {})
                    (assoc-in [var :obj] obj)
                    (assoc-in [var :var] var)
                    (assoc-in [var :obj?] (instance? OWLObjectProperty obj))
                    (assoc-in [var :domains] (map util/short-name (EntitySearcher/getDomains obj o)))
                    (assoc-in [var :ranges]  (map util/short-name (EntitySearcher/getRanges  obj o)))
                    (assoc-in [var :characteristics] (property-characteristics obj o)))))
            {} prop-vars)))
                    
(defn latex-write-onto!
  [out-file & root-concepts] ; This interns vars, including root-concepts.
  (with-open [wrtr (writer out-file)]
    (binding [*out* wrtr]
      (let [property-map (property-analysis)]
        (doall (map (fn [root-sym] ; e.g. onto/OperationsDomainConcept
                      (let [pc-map (util/onto-parent-child-map root-sym)]
                        (-> (util/onto-root-map {} [[(resolve root-sym)]] pc-map) 
                            util/vectify 
                            (latex-write-onto-nodes! property-map))))
                    root-concepts))))))


