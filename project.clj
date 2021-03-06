(defproject monoids-functors-monads "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]]
  :repl-options {:init-ns monoids-functors-monads.core}
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}

  :plugins [[lein-watch "0.0.3"]]
  :watch {:rate 500 ;; check file every 500ms ('watchtower' is used internally)
          :watchers {:compile {:watch-dirs ["src"]
                               :file-patterns [#"\.clj"]
                               :tasks ["compile"]}}})
