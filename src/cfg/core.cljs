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
       (for [insn (:instructions (om/props this))]
         (dom/tr {:key (str (:addr insn)) :class "insn"}
                 (dom/td {:class "addr"}
                         (hex-format (:addr insn)))
                 (dom/td {:class "padding-1"})
                 (dom/td {:class "bytes"}
                         (string/upper-case (:bytes insn)))
                 (dom/td {:class "padding-2"})
                 (dom/td {:class "mnem"}
                         (:mnem insn))
                 (dom/td {:class "padding-3"})
                 (dom/td {:class "operands"}
                         (:operands insn))
                 (dom/td {:class "padding-4"})
                 (dom/td {:class "comments"}
                         (when (and (:comments insn)
                                    (not= "" (:comments insn)))
                           (str ";  " (:comments insn))))))))))))


(def basicblock (om/factory BasicBlock))


(defui FunctionList
  Object
  (render
   [this]
   (let [props (om/props this)
         functions (:functions props)
         functions (sort-by :name functions)
         on-select-function (:select-function (om/get-computed this))]
     (dom/div
      {:class "function-list"}
      (dom/h3 {:class "title"}
              "functions (" (count functions) " total):")
      (dom/ul
       (for [function functions]
         (dom/li {:key (str (:offset function))
                  :class "function"
                  :onClick #(on-select-function (:offset function))}
                 (dom/span {:class "offset"}
                           (hex-format (:offset function)))
                 ": "
                 (dom/span {:class "name"}
                           (:name function)))))))))

(def function-list (om/factory FunctionList))


(defui BasicBlockList
  Object
  (render
   [this]
   (let [props (om/props this)
         bbs (:basic-blocks props)
         bbs (sort-by :addr bbs)
         on-select-bb (:select-bb (om/get-computed this))]
     (dom/div
      {:class "bb-list"}
      (dom/h3 {:class "title"}
              "basic blocks (" (count bbs) " total):")
      (dom/ul
       (for [bb bbs]
         (dom/li {:key (str (:addr bb))
                  :class "bb"
                  :onClick #(on-select-bb (:addr bb))}
                 (dom/span {:class "offset"}
                           (hex-format (:addr bb))))))))))

(def basic-block-list (om/factory BasicBlockList))


(def *model* (atom {:functions {}
                    :basic-blocks {}}))
(declare update-model!)


(defn r2->insn
  [insn]
  {:addr (:addr insn)
   :bytes (:bytes insn)
   :mnem (:mnemonic insn)
   :operands (subs (:opcode insn) (inc (count (:mnemonic insn))))
   :comments nil})


(defn layout-cfg
  [basic-blocks]
  (map-indexed (fn [i bb]
                 (assoc bb :x i :y i :width 0 :height 0))
               basic-blocks))


(defn positioned
  [props children]
  (let [x (:x props)
        y (:y props)
        top (str y "em")
        left (str x "em")]
    (dom/div {:class "laid-out"
              :style {:top top
                      :left left
                      :color "blue"}}
             children)))


(defui App
  Object
  (render
   [this]
   (dom/div
    {:class "app"}
    (dom/div
     {:class "panels"}
     (function-list
      (om/computed {:functions (vals (:functions (om/props this)))}
                   {:select-function (fn [fva]
                                       (update-model! {:selected-function fva})
                                       (go
                                         (let [afbj (<! (r2/get-basic-blocks fva))
                                               basic-blocks (:response afbj)]
                                           (update-model! [:functions fva :basic-blocks] basic-blocks)
                                           (doseq [basic-block basic-blocks]
                                             (go
                                               (let [addr (:addr basic-block)
                                                     ninstr (:ninstr basic-block)
                                                     aoj (<! (r2/get-instructions addr ninstr))
                                                     insns (map r2->insn (:response aoj))
                                                     changes (assoc basic-block :instructions insns)]
                                                 (update-model! [:basic-blocks addr] changes)))))))}))
     (let [props (om/props this)]
       (when (:selected-function props)
         (let [fva (:selected-function props)
               function (get-in props [:functions fva])
               basic-blocks (:basic-blocks function)]
           (basic-block-list
            (om/computed {:basic-blocks basic-blocks}
                         {:select-bb #(update-model! {:selected-basic-block %})}))))))
    (when (:selected-function (om/props this))
      (let [props (om/props this)
            fva (:selected-function props)
            function (get-in props [:functions fva])
            basic-blocks (:basic-blocks function)
            basic-blocks (map #(get-in props [:basic-blocks (:addr %)]) basic-blocks)
            basic-blocks (layout-cfg basic-blocks)]
        (canvas
         {}
         (for [bb basic-blocks]
           (positioned bb (basicblock bb)))))))))


(def app (om/factory App))


(defn- render!
  ([model]
   (prn "render!")
   (js/ReactDOM.render
    (app @model)
    (gdom/getElement "app")))
  ([model changes]
   (swap! model (fn [model]
                  (merge model changes)))
   (render! model))
  ([model path changes]
   (swap! model (fn [model]
                  (update-in model path (fn [cur]
                                          (if cur
                                            (merge cur changes)
                                            changes)))))
   (render! model)))


(defn update-model!
  ([new-stuff]
   (render! *model* new-stuff))
  ([path new-stuff]
   (render! *model* path new-stuff)))



(render! *model*)


(defn ensure-init
  []
  (let [ret (chan)]
    (go
      (let [aflj (<! (r2/get-functions))]
        (if (= :success (:status aflj))
          (do
            (prn "already init'd")
            (put! ret true)) ;
          (let [_ (prn "not yet init'd")
                _ (prn "initializing...")
                aaaa (<! (r2/analyze-all))
                _ (prn "initialized!")]
            (put! ret true)))))
    ret))



(go
  (let [_ (<! (ensure-init))
        aflj (<! (r2/get-functions))]
    (update-model! [:functions] (cmn/index-by :offset (:response aflj)))))
