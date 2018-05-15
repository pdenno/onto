(defproject modelmeth/onto "0.1.0-SNAPSHOT"
  :description "OWL Ontology utilities"
  :url "http://modelmeth.nist.gov"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure        "1.9.0"]
                 [org.clojure/core.logic     "0.8.11"] 
               #_[uk.org.russet/tawny-owl   "1.6.0"]
                 [uk.org.russet/tawny-owl    "2.0.0-SNAPSHOT-pod"]]
  :repl-options {:init-ns modelmeth.util})
                 

