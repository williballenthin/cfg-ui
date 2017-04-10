(ns cfg.layout.klay
  (:require [cfg.common :as cmn]))


(def klay (js* "$klay"))


(defn make
  []
  (clj->js {"id" "root"
            "properties" {"direction" "DOWN"
                          "spacing" 10
                          "edgeSpacingFactor" 1}
            "children" []
            "edges" []}))


(defn- cfg-bb->klay
  [bb]
  (clj->js {"id" (str (:addr bb))
            "width" (:width bb)
            "height" (:height bb)
            "properties" {"de.cau.cs.kieler.portConstraints" "FIXED_SIDE"}
            "ports" [{"id" (str (:addr bb) "IN")
                      "properties" {"de.cau.cs.kieler.portSide" "NORTH"}}
                     {"id" (str (:addr bb) "OUT")
                      "properties" {"de.cau.cs.kieler.portSide" "SOUTH"}}]}))



(defn add-node!
  [g bb]
  (let [nodes (aget g "children")]
    (.push nodes (cfg-bb->klay bb))))


(defn- cfg-edge->klay
  [edge]
  (clj->js {"source" (str (:src edge))
            "sourcePort" (str (:src edge) "OUT")
            "target" (str (:dst edge))
            "targetPort" (str (:dst edge) "IN")
            "type" (:type edge)
            "id" (str (:src edge) (:type edge) (:dst edge))}))


(defn add-edge!
  [g edge]
  (let [edges (aget g "edges")]
    (.push edges (cfg-edge->klay edge))))


(defn- klay-bb->cfg
  [bb]
  (let [x (get bb "x")
        y (get bb "y")
        w (get bb "width")
        h (get bb "height")]
    {
     :x x
     :y y
     :height h
     :width w
     :id (js/parseInt (get bb "id"))}))


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
  (let [layout-fn (aget klay "layout")]
    (layout-fn (clj->js {"graph" g
                         "options" {}
                         "success" s
                         "error" e}))))
