(ns modelmeth.core
  "Manipulate OWL ontologies"
  {:author "Peter Denno"}
  (:require [tawny.owl :as owl]
            [modelmeth.util :as util]
            [modelmeth.latex :as latex]))

(swap! util/+params+ #(assoc % :onto-namespace 'onto))

(def ontos "A map of the ontologies used in this project"
  (reduce-kv (fn [ontos k v]
               (binding [*ns* (-> k name symbol create-ns)]
                 ;(dosync (alter owl/ontology-for-namespace #(dissoc % *ns*)))
                 (-> ontos
                     (assoc k v)
                     (assoc-in [k :onto] (util/read-onto v "http://modelmeth.nist.gov/ops"))
                     (assoc-in [k :ns] *ns*))))
             {}
             {:vaem       {:iri "http://www.linkedmodel.org/schema/vaem"
                           :location "resources/vaem.owl"}
              :dtype      {:iri "http://www.linkedmodel.org/schema/dtype"
                           :location "resources/dtype.owl"}
              :qudt       {:iri "http://qudt.org/2.0/schema/qudt"
                           :location "resources/SCHEMA_QUDT-v2.0.ttl"}
              :modeling   {:iri "http://modelmeth.nist.gov/modeling"
                           :location "resources/modeling.ttl"}
              :operations {:iri "http://modelmeth.nist.gov/operations"
                           :location "resources/operations.ttl"}}))

(defn ops
  "Write the operations ontology."
  []
  (latex/latex-write-onto!
   "./output/operations-ontology.tex"
   (-> ontos :operations :ns)
   'onto/OperationsDomainConcept))

(defn modi
  "Write the modeling ontology."
  []
  (latex/latex-write-onto!
   "./output/modeling-ontology.tex"
   (-> ontos :modeling :ns)
   'onto/Abstract  'onto/Physical))




