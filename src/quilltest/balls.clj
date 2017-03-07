(ns quilltest.balls
  (:require [clojure.pprint :as pprint]
            [clojure.set :as set]
            [quil.core :as q]
            [quilltest.collision :as collision]
            [quilltest.core :as qcore]
            [quilltest.keys :as k]
            [quilltest.physics :as p]
            [quilltest.scene :as scene])
  (:gen-class))

;(-main)
(def params
  {:size [800 600]
   :fps 15
   :update-fps 500
   :balls 60})

(def state-atom
  (letfn [(px []
            [(rand-int (first (:size params)))
             (rand-int (second (:size params)))])]
      (atom
       (map (fn [id]
              {:pos (px)
               :velocity [0.01 0.01]
               :id id})
            (range (:balls params))))))

(defn scene-graph-node
  []
  (scene/->GraphNode
   []
   (fn [[x y]]
     (q/ellipse x y 20 20))))

(def keys-atom (atom #{}))

(defn going-out? [pos max-pos vel]
  (or (and (> pos max-pos) (pos? vel))
      (and (< pos 0) (neg? vel))))

(defn bounds-check
  "Prevent balls from flying out of the field"
  [state]
  (let [[max-x max-y] (:size params)
        [vx vy] (:velocity state)
        [x y] (:pos state)
        vx (if (going-out? x max-x vx) (- vx) vx)
        vy (if (going-out? y max-y vy) (- vy) vy)]
    [vx vy]))

(def time-scale 0.00001)
(def ACCEL 0.000001)

(defn accelerate [t vx vy]
  "v = a*t + v0; Allows overriding of acceleration via keypress"
  (let [step (* ACCEL t)
        pressed @keys-atom
        vx (if (pressed :a) (- vx step) vx)
        vx (if (pressed :d) (+ vx step) vx)
        vy (if (pressed :w) (- vy step) vy)
        vy (if (pressed :s) (+ vy step) vy)]
    [vx vy]))

(defn move [node ticks]
  (let [t (* time-scale ticks)
        [vx vy] (bounds-check node)
        [vx vy] (accelerate t vx vy)
        [vxt vyt] [(* vx t) (* vy t)]
        [x y] (:pos node)]
    {;; x = vt + x0
     :pos [(+ x vxt) (+ y vyt)]
     :velocity [vx vy]}))

(defn setup []
  (q/frame-rate (:fps params))
  (q/stroke-weight 2)
  (q/smooth))

(defn draw []
  (q/background 100)
  (let [s @state-atom]
    (q/stroke 0)
    (dorun (map #(scene/draw (scene-graph-node) (:pos %)) s))))

(defn node->rigidbody [node]
  (let [{:keys [pos velocity]} node]
    (p/->RigidBody
     1
     (p/->Vector2 (first pos) (second pos))
     (p/->Vector2 (first velocity) (second velocity)))))

(defn rigidbody->node [rigidbody]
  {:pos (let [pos (:position rigidbody)]
          [(:x pos) (:y pos)])
   :velocity (let [v (:velocity rigidbody)]
               [(:x v) (:y v)])})

(defn update-nodes
  "update the game state of the nodes post-collision"
  [nodes]
  (map
   (fn [node]
     ;; if there is any state coming from the old node, add
     ;; it here
     (let [new-state (rigidbody->node (:rigid-body node))]
       (merge node new-state))) nodes))

(defn update-position [nodes ticks]
  (let [;;add a rigid-body key
        nodes (map #(assoc % :rigid-body (node->rigidbody %)) nodes)
        nodes (collision/update-collisions nodes)
        nodes (update-nodes nodes)]
    (map #(move % ticks) nodes)))

(defn -main []
  (qcore/run-sketch {:title "Balls"
                     :setup #'setup
                     :draw #'draw
                     :size (:size params)}
                    {:on-key-press (k/gen-on-keypress keys-atom)
                     :on-key-release (k/gen-on-keyrelease keys-atom)}
                    (:update-fps params)
                    (fn [ticks] (swap! state-atom #(update-position % ticks)))
                    #(when (:q @keys-atom) true)))

;;(-main)

