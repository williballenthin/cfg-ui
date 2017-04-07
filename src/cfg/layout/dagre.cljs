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


(defn- scale-node-props
  [bb]
  {:x (scale-down (get bb "x"))
   :y (scale-down (get bb "y"))
   :height (scale-down (get bb "height"))
   :width (scale-down (get bb "width"))
   :label (js/parseInt (get bb "label"))})


(defn get-nodes
  [g]
  (map scale-node-props (vals (js->clj (aget g "_nodes")))))


(defn- scale-point-props
  [point]
  {:x (scale-down (get point "x"))
   :y (scale-down (get point "y"))})


(defn- scale-edge-props
  [edge]
  {:points (mapv scale-point-props (get edge "points"))})


(defn get-edges
  [g]
  (mapv scale-edge-props (vals (js->clj (aget g "_edgeLabels")))))


(def layout! (aget dagre "layout"))
