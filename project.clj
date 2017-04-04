(defproject cfg "0.1.0-SNAPSHOT"
  :description "control flow graph user interface experiments"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 ;; 1.9.36 is the last version we can use before chrome gets mad
                 ;;  about synchronous ajax requests in the main thread.
                 [org.clojure/clojurescript "1.9.36"]
                 [org.omcljs/om "1.0.0-alpha47" :exclusions [cljsjs/react]]
                 ;; this one doesn't seem to work with om-alpha47
                                        ;[cljsjs/react-with-addons "15.4.2-2"]
                 [cljsjs/react-with-addons "15.3.1-0"]
                 [org.clojars.drcode/blueprint-cljs "0.1.1-SNAPSHOT"]
                 [figwheel-sidecar "0.5.9-SNAPSHOT" :scope "test"]
                 [cljs-ajax "0.5.8"]
                 [prismatic/om-tools "0.4.0"]
                 [binaryage/devtools "0.9.2"]]
  :plugins [[lein-cljsbuild "1.1.5"]]
  :cljsbuild {
              :builds [{
                        ;; The path to the top-level ClojureScript source directory:
                        :source-paths ["src"]
                        ;; The standard ClojureScript compiler options:
                        ;; (See the ClojureScript compiler documentation for details.)
                        :compiler {
                                   :asset-path "js"
                                   :output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js"
                                   :verbose true
                                   :main 'cfg.core
                                   :optimizations :whitespace
                                   :pretty-print true}}]})
