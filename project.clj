(defproject clj-cdt "0.0.1"
  :description "Library for parsing C/C++"
  :license {:name "The MIT License (MIT)"
            :url  "http://opensource.org/licenses/MIT"}
  :dependencies [
                 [org.clojure/clojure "1.9.0-alpha4"]
                 [org.clojure/test.check "0.9.0"]
                 [prismatic/schema "1.1.3"]
                 [swiss-arrows "1.0.0"]
                 ]
  :resource-paths ["./resources/org.eclipse.cdt.core_6.4.0.201803220800.jar"
                   "./resources/org.eclipse.core.resources_3.12.0.v20170417-1558.jar"
                   "./resources/org.eclipse.core.runtime_3.13.0.v20170207-1030.jar"
                   "./resources/org.eclipse.equinox.common_3.9.0.v20170207-1454.jar"
                   "./resources/org.eclipse.osgi_3.12.100.v20180210-1608.jar"
                   "./resources/org.eclipse.core.jobs_3.9.3.v20180115-1757.jar"
                   "src/test/resources"
                   ]
  :java-source-paths ["src/java"]
  :main clj-cdt.core
  :plugins [[lein-codox "0.10.3"]]
)
