(defproject clj-cdt "0.0.1"
  :description "Library for parsing C/C++"
  :license {:name "The MIT License (MIT)"
            :url  "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha4"]
                 [org.clojure/test.check "0.9.0"]
                 [prismatic/schema "1.1.3"]
                 [swiss-arrows "1.0.0"]
                 [org.eclipse.core/org.eclipse.core.resources "3.6.0.v20100526-0737"]
                 ]
  :resource-paths [;"resources/org.eclipse.cdt.core_6.4.0.201802122019.jar"
                   "resources/org.eclipse.cdt.core_6.2.0.201612061315.jar"
                   "src/test/resources"
                   ]
  :java-source-paths ["src/java"]
  :main clj-cdt.core
  :plugins [[lein-codox "0.10.3"]]
)
