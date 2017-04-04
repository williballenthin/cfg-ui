(ns cfg.common
  (:require [om.next :as om]
            [cljs.pprint]))

(defn pp
  "via: http://stackoverflow.com/a/32108640/87207"
  [s]
  (with-out-str (cljs.pprint/pprint s)))

(defn d
  "
    Log a debug message to the console.
    With cljs-devtools installed, things are formatted nicely.
  "
  [msg]
  (.log js/console msg))
