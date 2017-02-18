(defproject clojure-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [com.rpl/specter "0.13.0"]
                 [mvxcvi/puget "1.0.1"]
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 ;; [org.skummet/clojure "1.7.0-r2"]
                 [swiss-arrows "1.0.0"]
                 [com.cemerick/pomegranate "0.3.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                ]
  :main ^:skip-aot clojure-test.core
  :target-path "target/%s"
  :jvm-opts ["-Xmx200m" "-server"]
  :repl-options {:port 5000}
  ;; :jvm-opts ["-Xmx200m" "-server" "-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"]
  :profiles {:uberjar {:aot :all}}
  )
