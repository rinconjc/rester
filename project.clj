(defproject rester "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.3"]
                 [clj-http "2.2.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 [cheshire "5.6.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [json-path "0.2.0"]
                 [dk.ative/docjure "1.10.0"]]
  :main ^:skip-aot rester.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
