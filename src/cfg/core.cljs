(ns cfg.core
  (:require [goog.dom :as gdom]
            [clojure.data]
            [clojure.string :as string]
            [cljs.core.async :refer [put! chan <! close!]]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs.pprint]
            [om.util]
            [om-tools.dom :as dom]
            [om.next :as om :refer-macros [defui]]
            ;; include this first so it gets installed early
            [cfg.devtools :as cfg.devtools]
            [cfg.common :as cmn]
            [cfg.api :as r2])
  (:require-macros [cljs.core.async.macros :refer [go]]))


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
   ;; use local mutable state for performance.
   ;; while panning, update this state, and only re-render upon `onMouseUp`.
   (let [a-last-x (atom (:last-x (om/get-state this)))
         a-margin-left (atom (:margin-left (om/get-state this)))
         a-last-y (atom (:last-y (om/get-state this)))
         a-margin-top (atom (:margin-top (om/get-state this)))]
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
                       state (om/get-state this)
                       updates {:dragging false
                                :last-x @a-last-x
                                :margin-left @a-margin-left
                                :last-y @a-last-y
                                :margin-top @a-margin-top}]
                   (om/set-state! this (merge state updates))))
               :onMouseMove
               (fn [e]
                 (.preventDefault e)
                 (when (:dragging (om/get-state this))
                   (let [evt (or e (js/event))
                         canvas (js/ReactDOM.findDOMNode (aget this "refs" "canvas"))
                         style (aget canvas "style")
                         client-x (aget evt "clientX")
                         delta-x (- client-x @a-last-x)
                         margin-left' (+ @a-margin-left delta-x)
                         client-y (aget evt "clientY")
                         delta-y (- client-y @a-last-y)
                         margin-top' (+ @a-margin-top delta-y)]
                     (reset! a-last-x client-x)
                     (reset! a-margin-left margin-left')
                     (reset! a-last-y client-y)
                     (reset! a-margin-top margin-top')
                     (aset style "marginLeft" (str margin-left' "px"))
                     (aset style "marginTop" (str margin-top' "px")))))}
              ;;(om/set-state! this (merge state updates)))))}
              (dom/div {:class "canvas"
                        :ref "canvas"
                        :style #js{"marginLeft" (:margin-left (om/get-state this))
                                   "marginTop" (:margin-top (om/get-state this))}}
                       (om/children this))))))


(def canvas (om/factory Canvas))


(defn hex-format
  [n]
  (str "0x" (string/upper-case (.toString n 16))))


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
                 (dom/td {:class "addr"}
                         (hex-format (:addr insn)))
                 (dom/td {:class "bytes"}
                         (string/upper-case (:bytes insn)))
                 (dom/td {:class "mnem"}
                         (:mnem insn))
                 (dom/td {:class "operands"}
                         (:operands insn))
                 (dom/td {:class "comments"}
                         (when (and (:comments insn)
                                    (not= "" (:comments insn)))
                           (str ";  " (:comments insn))))))))))))


(def basicblock (om/factory BasicBlock))


(defui FunctionList
  Object
  (render
   [this]
   (dom/div
    {:class "function-list"}
    (dom/ul
     (for [function (sort-by :name (:functions (om/props this)))]
       (dom/li {:key (str (:offset function))
                :class "function"}
               (dom/span {:class "offset"}
                         (hex-format (:offset function)))
               ": "
               (dom/span {:class "name"}
                         (:name function))))))))


(def function-list (om/factory FunctionList))


(defui App
  Object
  (render
   [this]
   (dom/div
    {:class "app"}
    (function-list (om/props this))
    (canvas
     {:props :none}
     (basicblock {:insns [{:addr 0x412B4F :bytes "53" :mnem "push" :operands "ebx"}
                          {:addr 0x412b50 :bytes "6A 01" :mnem "push" :operands "1" :comments "size_t"}
                          {:addr 0x412b52 :bytes "e8 06 18 00 00" :mnem "call" :operands "??2@YAPAXI@Z" :comments "operator new(uint)"}
                          {:addr 0x412b57 :bytes "8b d8" :mnem "cmov" :operands "ebx, eax"}]})))))


(def app (om/factory App))


(defn- render!
  ([model]
   (cmn/d "render!")
   (js/ReactDOM.render
     (app @model)
     (gdom/getElement "app")))
  ([model changes]
   (swap! model merge changes)
   (render! model)))

(render! (atom {}))


(defn ensure-init
  []
  (let [ret (chan)]
    (go
      (let [aflj (<! (r2/get-functions2))]
        (if (= :success (:status aflj))
          (do
            (prn "already init'd")
            (put! ret true)) ;
          (let [_ (prn "not yet init'd")
                _ (prn "initializing...")
                aaaa (<! (r2/analyze-all2))
                _ (prn "initialized!")]
            (put! ret true)))))
    ret))


(let [model (atom {})]
  (go
    (let [_ (<! (ensure-init))
          aflj (<! (r2/get-functions2))]
      (render! model {:functions (:response aflj)}))))
