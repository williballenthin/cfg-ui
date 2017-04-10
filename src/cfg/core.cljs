(ns cfg.core
  (:require [goog.dom :as gdom]
            [clojure.data]
            [clojure.string :as string]
            [cljs.core.async :refer [put! chan <! close!]]
            [goog.string :as gstring]
            [goog.string.format]
            [cljs.pprint]
            [om-tools.dom :as dom]
            [om.next :as om :refer-macros [defui]]
            ;; include this first so it gets installed early
            [cfg.devtools :as cfg.devtools]
            [cfg.common :as cmn]
            [cfg.api :as r2]
            [cfg.layout.dagre :as dagre]
            [cfg.layout.klay :as klay])
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


(defn basicblock
  [props]
  (dom/div
   {:class "basic-block"}
   (dom/div {:class "bb-header"})
   (dom/div
    {:class "bb-content"}
    (dom/table
     (dom/thead)
     (dom/tbody
      (for [insn (:instructions props)]
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
                          (str ";  " (:comments insn)))))))))))



(defn function-list
  [props]
  (let [functions (:functions props)
        functions (sort-by :name functions)
        on-select-function (:select-function props)]
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
                          (:name function))
                " ("
                (dom/span {:class "basic-block-count"}
                          (:nbbs function))
                ")"))))))


(defn basic-block-list
  [props]
  (let [bbs (:basic-blocks props)
        bbs (sort-by :addr bbs)
        on-select-bb (:select-bb props)]
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
                          (hex-format (:addr bb)))))))))


(def sqrt (.-sqrt js/Math))
(def PI (.-PI js/Math))
(def atan2 (.-atan2 js/Math))


;; these line drawing algorithms ripped directly from:
;;  http://stackoverflow.com/questions/4270485/drawing-lines-on-html-page

(defn geoline
  [x y length angle]
  (dom/div
   {:class "line"
    :style {:width (str length "em")
            :transform (str "rotate(" angle "rad)")
            :top (str y "em")
            :left (str x "em")}}))


(defn line
  [x2 y2 x1 y1]
  (let [a (- x1 x2)
        b (- y1 y2)
        c (sqrt
           (+
            (* a a)
            (* b b)))
        sx (/ (+ x1 x2) 2)
        sy (/ (+ y1 y2) 2)
        x (- sx (/ c 2))
        y sy
        alpha (- PI (atan2 (- b) a))]
    (geoline x y c alpha)))


(defn multi-line
  [props children]
  (let [class (:class props)
        class (if class
                (str "multi-line " class)
                "multi-line")]
    (dom/div {:class class}
             children)))


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


(defn compute-bb-height
  "
  units: em
  "
  [bb]
  (let [insn-count (count (:instructions bb))
        ;; assume header size is 1em,
        ;; which is defined in the css style.
        header-size 1]
    (+ header-size insn-count)))


(defn compute-bb-width
  "
  units: em
  "
  [bb]
  ;; the following constants are defined in the css style.
  (let [padding-1-size 1
        padding-2-size 1
        padding-3-size 1
        padding-4-size 1
        bytes-size 12
        mnem-size 6
        operands-size (apply max (map #(count (:operands %)) (:instructions bb)))
        comments-size (apply max (map #(count (:comments %)) (:instructions bb)))]
    (+ padding-1-size
       padding-2-size
       padding-3-size
       padding-4-size
       bytes-size
       mnem-size
       operands-size)))


(defn compute-edges
  [basic-blocks]
  (remove nil?
          (concat
            (for [bb basic-blocks]
              (when (:jump bb)
                {:src (:addr bb) :dst (:jump bb) :type :jump}))
            (for [bb basic-blocks]
              (when (:fail bb)
                {:src (:addr bb) :dst (:fail bb) :type :fail})))))


(defn dump-edges
  [edges]
  (doseq [edge edges]
    (prn (str (:type edge) " " (hex-format (:src edge)) " -> " (hex-format (:dst edge))))))


(defn layout-bbs
  "
   Using the given layout, merge the x and y coordinates onto the given basic blocks.

   Params:
    layout (map): from int basic block address to position map, with keys :x and :y.
    bbs (sequence): the basic blocks.


   Example::

       (let [nodes       (klay/get-nodes result)
             nodes-by-id (cmn/index-by :id nodes)
             laid-out    (layout-bbs nodes-by-id basic-blocks)]
         ...

  "
  [layout bbs]
  (for [bb bbs]
    (let [pos (get layout (:addr bb))]
      (merge bb {:x (:x pos)
                 :y (:y pos)}))))


(defn layout-cfg
  [basic-blocks s e]
  (when (< 0 (count (remove nil? basic-blocks)))
    (let [edges (compute-edges basic-blocks)
          bbs (map #(assoc % :width (compute-bb-width %)) basic-blocks)
          bbs (map #(assoc % :height (compute-bb-height %)) bbs)
          g (dagre/make)
          g2 (klay/make)]
      (dump-edges edges)
      (doseq [bb bbs]
        (dagre/add-node! g bb)
        (klay/add-node! g2 bb))
      (doseq [edge edges]
        (dagre/add-edge! g edge)
        (klay/add-edge! g2 edge))
      (dagre/layout! g)
      (klay/layout g2
                   (fn [r]
                     (let [edges (klay/get-edges r)]
                       (prn "klay: success!")
                       (cmn/d r)
                       (cmn/d edges)
                       (s {:nodes (layout-bbs (cmn/index-by :id (klay/get-nodes r)) bbs)
                           :edges (klay/get-edges r)})))
                   (fn [err]
                     (prn "klay: error")
                     (cmn/d err)
                     (e err)))
      (s {:nodes (layout-bbs (cmn/index-by :label (dagre/get-nodes g)) bbs)
          ;; TODO: recover edge src, dst
          :edges (dagre/get-edges g)}))))


(defn positioned
  [props children]
  (let [x (:x props)
        y (:y props)
        w (:width props)
        h (:height props)
        top (str (- y (/ h 2)) "em")
        left (str (- x  (/ w 2)) "em")]
    (dom/div {:class "laid-out"
              :style {:top top
                      :left left}}
             children)))


(defn edge-line
  [edge]
  (multi-line
   {:class (condp = (:type edge)
             :fail "edge-false"
             "edge-true")}
   (for [pair (partition 2 1 (:points edge))]
     (let [start (first pair)
           end (second pair)
           x1 (:x start)
           y1 (:y start)
           x2 (:x end)
           y2 (:y end)]
       (line x1 y1 x2 y2)))))


(defn app
  [props]
  (dom/div
   {:class "app"}
   (dom/div
    {:class "panels"}
    (function-list
     {:functions (vals (:functions props))
      :select-function (fn [fva]
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
                                   (update-model! [:basic-blocks addr] changes)))))))})
    (when (:selected-function props)
      (let [fva (:selected-function props)
            function (get-in props [:functions fva])
            basic-blocks (:basic-blocks function)]
        (basic-block-list {:basic-blocks basic-blocks
                           :select-bb #(update-model! {:selected-basic-block %})}))))
   (when (:selected-function props)
     (let [fva (:selected-function props)
           function (get-in props [:functions fva])
           basic-blocks (:basic-blocks function)
           basic-blocks (map #(get-in props [:basic-blocks (:addr %)]) basic-blocks)
           g (layout-cfg basic-blocks (fn [layout] (cmn/d layout)) (fn [err] (cmn/d err)))]
       (canvas
        {}
        [(for [bb (:nodes g)]
           (positioned bb (basicblock bb)))
         (for [edge (:edges g)]
           (edge-line edge))])))))


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
