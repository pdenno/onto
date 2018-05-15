(ns modelmeth.core
  "Manipulate OWL ontologies"
  {:author "Peter Denno"}
  (:require [tawny.owl :as owl]
            [modelmeth.util :as util :refer [with-onto ppp ppprint]]
            [modelmeth.latex :as latex]))

(swap! util/+params+ #(assoc % :onto-namespace 'onto))

;;; Probably want to do import and not all of this stuff. 
(def ontos "A map of the ontologies used in this project"
  (reduce-kv (fn [ont k v]
               (binding [*ns* (-> k name symbol create-ns)]
                 ;;(dosync (alter owl/ontology-for-namespace #(dissoc % *ns*)))
                 (as-> ont ?ont
                     (assoc ?ont k v)
                     (assoc-in ?ont [k :ns] *ns*)
                     (assoc-in ?ont [k :name] k)
                     (assoc-in ?ont [k :onto] (util/read-onto (get ?ont k))))))
             {}
             {:vaem       {:iri "http://www.linkedmodel.org/schema/vaem"
                           :location "resources/vaem.owl"}
              :dtype      {:iri "http://www.linkedmodel.org/schema/dtype"
                           :location "resources/dtype.owl"}
              :qudt       {:iri "http://qudt.org/2.0/schema/qudt"
                           :location "resources/SCHEMA_QUDT-v2.0.ttl"}
              :modl       {:iri "http://modelmeth.nist.gov/modeling"
                           :location "resources/modeling.ttl"}
              :ops        {:iri "http://modelmeth.nist.gov/operations"
                           :location "resources/operations.ttl"}}))


;;; Probably not necessary?
#_(binding [*ns* (find-ns 'ops)]
  (doseq [imp [:vaem :dtype :qudt :modeling]]
    (owl/owl-import (-> ontos imp :onto))))

(defn ops
  "Write the operations ontology."
  []
  (with-onto :ops
    (latex/latex-write-onto!
     "./output/operations-ontology.tex"
     'ops/OperationsDomainConcept)))

(defn modi
  "Write the modeling ontology."
  []
  (latex/latex-write-onto!
   "./output/modeling-ontology.tex"
   (-> ontos :modeling :ns)
   'modl/Abstract  'modl/Physical))




