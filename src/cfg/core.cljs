(ns cfg.core
  (:require [goog.dom :as gdom]
            [clojure.data]
            [clojure.string :as string]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs.pprint]
            [om.util]
            [om-tools.dom :as dom]
            [om.next :as om :refer-macros [defui]]
            ;; include this first so it gets installed early
            [cfg.devtools :as cfg.devtools]
            [cfg.common :as cmn]))


(enable-console-print!)


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
   (dom/div {:class "canvas-viewport"
             ;; click-and-drag on the viewport pans the canvas
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
            (dom/div {:class "canvas"
                      :style #js{"marginLeft" (:margin-left (om/get-state this))
                                 "marginTop" (:margin-top (om/get-state this))}}
                     (om/children this)))))


(def canvas (om/factory Canvas))

(defn hex-format
  [n]
  (.toString n 16))



(defui BasicBlock
  Object
  (render
   [this]
   (dom/div
    {:class "basic-block"}
    (dom/div {:class "bb-header"})
    (dom/div
     {:class "bb-content"}
     (dom/table
      (dom/thead)
      (dom/tbody
       (for [insn (:insns (om/props this))]
         (dom/tr {:key (str (:addr insn)) :class "insn"}
                 (dom/td {:class "addr"} (str "0x" (string/upper-case (hex-format (:addr insn)))))
                 (dom/td {:class "bytes"} (string/upper-case (:bytes insn)))
                 (dom/td {:class "mnem"} (:mnem insn))
                 (dom/td {:class "operands"} (:operands insn))
                 (dom/td {:class "comments"} (when (and (:comments insn)
                                                        (not= "" (:comments insn)))
                                               (str ";  " (:comments insn))))))))))))


(def basicblock (om/factory BasicBlock))


(defui App
  Object
  (render
   [this]
   (canvas
    {:props :none}
    (basicblock {:insns [{:addr 0x412B4F :bytes "53" :mnem "push" :operands "ebx"}
                         {:addr 0x412b50 :bytes "6A 01" :mnem "push" :operands "1" :comments "size_t"}
                         {:addr 0x412b52 :bytes "e8 06 18 00 00" :mnem "call" :operands "??2@YAPAXI@Z" :comments "operator new(uint)"}
                         {:addr 0x412b57 :bytes "8b d8" :mnem "cmov" :operands "ebx, eax"}]}))))


(def app (om/factory App))


(js/ReactDOM.render
 (app {})
 (gdom/getElement "app"))
