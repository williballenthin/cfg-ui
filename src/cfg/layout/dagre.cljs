(ns cfg.layout.dagre
  (:require [cfg.common :as cmn]))


(def dagre (js* "dagre"))


(defn make
  []
  (let [Graph (aget dagre "graphlib" "Graph")
        g (Graph.)]
    (.setGraph g #js{"nodesep" 5
                     "edgesep" 10
                     "ranksep" 10})
    (.setDefaultEdgeLabel g (fn [x] #js{}))
    g))


(defn- cfg-bb->dagre
  [bb]
  #js{"width" (:width bb)
      "height" (:height bb)
      "label" (str (:addr bb))})


(defn add-node!
  [g bb]
  (let [bb' (cfg-bb->dagre bb)]
    (.setNode g (aget bb' "label") bb')))


(defn add-edge!
  [g edge]
  (.setEdge g (str (:src edge)) (str (:dst edge)) #js{"type" (:type edge)}))


(defn- dagre-bb->cfg
  [bb]
  {:x (get bb "x")
   :y (get bb "y")
   :height (get bb "height")
   :width (get bb "width")
   :label (js/parseInt (get bb "label"))})


(defn get-nodes
  [g]
  (map dagre-bb->cfg (vals (js->clj (aget g "_nodes")))))


(defn- dagre-point->cfg
  [point]
  {:x (get point "x")
   :y (get point "y")})


(defn- dagre-edge->cfg
  [edge]
  {:points (mapv dagre-point->cfg (get edge "points"))
   :type (get edge "type")})


(defn get-edges
  [g]
  (mapv dagre-edge->cfg (vals (js->clj (aget g "_edgeLabels")))))


(def layout! (aget dagre "layout"))
