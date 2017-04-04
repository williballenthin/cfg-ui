(ns cfg.core
  (:require [goog.dom :as gdom]
            [clojure.data]
            [clojure.string :as string]
            [cljs.pprint]
            [om.util]
            [om.dom :as dom]
            [om.next :as om :refer-macros [defui]]
            ;; include this first so it gets installed early
            [cfg.devtools :as cfg.devtools]))


(enable-console-print!)

(println "hello world!")
