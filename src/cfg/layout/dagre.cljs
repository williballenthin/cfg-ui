(ns cfg.layout.dagre
  (:require [cfg.common :as cmn]))


(def dagre (js* "dagre"))


(defn make
  []
  (let [Graph (aget dagre "graphlib" "Graph")
        g (Graph.)]
    (.setGraph g #js{"nodesep" 100
                     "edgesep" 50
                     "ranksep" 75})
    (.setDefaultEdgeLabel g (fn [x] #js{}))
    g))


(def scale-const 13)


(defn- scale-up
  [n]
  (* scale-const n))


(defn- scale-down
  [n]
  (/ n scale-const))


(defn add-node!
  [g bb]
  (.setNode g (str (:addr bb)) #js{"width" (scale-up (:width bb))
                                   "height" (scale-up (:height bb))
                                   "label" (str (:addr bb))}))


(defn add-edge!
  [g edge]
  (.setEdge g (str (:src edge)) (str (:dst edge))))


(defn scale-props
  [bb]
  {:x (scale-down (get bb "x"))
   :y (scale-down (get bb "y"))
   :height (scale-down (get bb "height"))
   :width (scale-down (get bb "width"))
   :label (js/parseInt (get bb "label"))})


(defn get-nodes
  [g]
  (map scale-props (vals (js->clj (aget g "_nodes")))))


(defn get-edges
  [g]
  (vals (js->clj (aget g "_edgeLabels"))))


(def layout! (aget dagre "layout"))
