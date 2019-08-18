(defproject rester "0.2.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [clj-http "3.10.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 [cheshire "5.6.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [json-path "2.0.0"]
                 [dk.ative/docjure "1.10.0"]
                 [io.forward/yaml "1.0.9"]
                 [org.clojure/core.async "0.4.490"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [org.clojure/tools.cli "0.4.1"]
                 ;; [org.clojure/core.specs.alpha "0.2.44"]
                 ]
  :main ^:skip-aot rester.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[org.clojure/test.check "0.10.0-alpha3"]]}}
  :uberjar-name "rester.jar")
