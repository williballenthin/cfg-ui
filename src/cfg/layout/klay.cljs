(ns cfg.layout.klay
  (:require [cfg.common :as cmn]))


(def klay (js* "$klay"))


(defn make
  []
  #js{"id" "root"
      "properties" #js{"direction" "DOWN"
                       "spacing" 40}
      "children" #js[]
      "edges" #js[]})


(defn- cfg-bb->klay
  [bb]
  #js{"width" (:width bb)
      "height" (:height bb)
      "id" (str (:addr bb))})


(defn add-node!
  [g bb]
  (let [nodes (aget g "children")]
    (.push nodes (cfg-bb->klay bb))))


(defn- cfg-edge->klay
  [edge]
  #js{"source" (str (:src edge))
      "target" (str (:dst edge))
      "type" (:type edge)
      "id" (str (:src edge) (:type edge) (:dst edge))})


(defn add-edge!
  [g edge]
  (let [edges (aget g "edges")]
    (.push edges (cfg-edge->klay edge))))


(defn- klay-bb->cfg
  [bb]
  {:x (get bb "x")
   :y (get bb "y")
   :height (get bb "height")
   :width (get bb "width")
   :id (js/parseInt (get bb "id"))})


(defn get-nodes
  [g]
  (mapv klay-bb->cfg (js->clj (aget g "children"))))


(defn- klay-point->cfg
  [point]
  {:x (get point "x")
   :y (get point "y")})


(defn- klay-edge->cfg
  [edge]
  (let [src-point (get edge "sourcePoint")
        target-point (get edge "targetPoint")
        bend-points (get edge "bendPoints")
        points (concat [src-point] bend-points [target-point])]
    {:points (mapv klay-point->cfg points)
     :type (get edge "type")
     :src (js/parseInt (get edge "source"))
     :dst (js/parseInt (get edge "target"))}))


(defn get-edges
  [g]
  (mapv klay-edge->cfg (js->clj (aget g "edges"))))


(defn layout
  [g s e]
  (cmn/d g)
  (let [layout-fn (aget klay "layout")]
    (layout-fn #js{"graph" g
                   "options" #js{}
                   "success" s
                   "error" e})))
