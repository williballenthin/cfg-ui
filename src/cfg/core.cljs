(ns cfg.core
  (:require [goog.dom :as gdom]
            [clojure.data]
            [clojure.string :as string]
            [cljs.pprint]
            [om.util]
            [om-tools.dom :as dom]
            [om.next :as om :refer-macros [defui]]
            ;; include this first so it gets installed early
            [cfg.devtools :as cfg.devtools]
            [cfg.common :as cmn]))


(enable-console-print!)

(cmn/d "hello world!")




(defui Canvas
  Object
  (initLocalState
   [this]
   {:dragging false
    :last-x 0
    :margin-left 0
    :last-y 0
    :margin-top 0})
  (render
   [this]
   (dom/div {:class "canvas-viewport"}
            (dom/div {:class "canvas"
                      :style #js{"marginLeft" (:margin-left (om/get-state this))
                                 "marginTop" (:margin-top (om/get-state this))}
                      :onMouseDown
                      (fn [e]
                        (.preventDefault e)
                        (let [evt (or e (js/event))
                              last-x (aget evt "clientX")
                              last-y (aget evt "clientY")
                              state (om/get-state this)]
                          (om/set-state! this (assoc state :dragging true :last-x last-x :last-y last-y))))
                      :onMouseUp
                      (fn [e]
                        (.preventDefault e)
                        (let [evt (or e (js/event))
                              state (om/get-state this)]
                          (om/set-state! this (assoc state :dragging false))))
                      :onMouseMove
                      (fn [e]
                        (.preventDefault e)
                        (when (:dragging (om/get-state this))
                          (let [evt (or e (js/event))
                                state (om/get-state this)
                                client-x (aget evt "clientX")
                                delta-x (- client-x (:last-x state))
                                margin-left' (+ (:margin-left state) delta-x)

                                client-y (aget evt "clientY")
                                delta-y (- client-y (:last-y state))
                                margin-top' (+ (:margin-top state) delta-y)
                                updates {:last-x client-x
                                         :margin-left margin-left'
                                         :last-y client-y
                                         :margin-top margin-top'}]
                            (om/set-state! this (merge state updates)))))}
                     (om/children this)))))


(def canvas (om/factory Canvas))


(defui App
  Object
  (render
   [this]
   (canvas
    {:props :none}
    (dom/div "hello world!"))))

(def app (om/factory App))


(js/ReactDOM.render
 (app {})
 (gdom/getElement "app"))
