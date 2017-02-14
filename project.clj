(defproject clj-ray "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot clj-ray.core
  :target-path "target/%s"
  :global-vars {*warn-on-reflection* true
                *assert* false}
  :profiles {:uberjar {:aot :all}})
