(ns quilltest.balls
  (:require [quilltest.core :as qcore]
            [quilltest.keys :as k]
            [quilltest.scene :as scene]
            [quilltest.physics :as p])
  (:require [quil.core :as q])
  (:gen-class))

;(-main)
(def params
  {:size [800 600]
   :fps 61
   :update-fps 500})

(def state-atom (atom {:guy1 {:pos [101 101]
                              :velocity [-0.5 -0.5]}
                       :guy2 {:pos [400 100]
                              :velocity [0.5 0.5]}}))

(def scene-graph
  (let [guy (scene/->GraphNode
             []
             (fn [[x y]]
               (q/ellipse x y 20 20)))]
    guy))


(def keys-atom (atom #{}))

(defn going-out? [pos max-pos vel]
  (or (and (> pos max-pos) (pos? vel))
      (and (< pos 0) (neg? vel))))

(defn bounds-check [state]
  (let [[max-x max-y] (:size params)
        [vx vy] (:velocity state)
        [x y] (:pos state)
        vx (if (going-out? x max-x vx) (- vx) vx)
        vy (if (going-out? y max-y vy) (- vy) vy)]
    [vx vy]))

(def scale 0.000001)
(def a 0.001)

(defn accelerate [t vx vy]
  ;; v = a*t + v0
  (let [step (* a t)
        pressed @keys-atom
        vx (if (pressed :a) (- vx step) vx)
        vx (if (pressed :d) (+ vx step) vx)
        vy (if (pressed :w) (- vy step) vy)
        vy (if (pressed :s) (+ vy step) vy)]
    [vx vy]))

(defn move [state ticks]
  (let [t (* scale ticks)
        [vx vy] (bounds-check state)
        [vx vy] (accelerate t vx vy)
        [vxt vyt] [(* vx t) (* vy t)]
        [x y] (:pos state)]
    (assoc state
      ; x = vt + x0
      :pos [(+ x vxt) (+ y vyt)]
      :velocity [vx vy])))

(defn setup []
  (q/frame-rate (:fps params))
  (q/stroke-weight 2)
  (q/smooth))

(defn draw []
  (q/background 100)
  (let [s @state-atom
        {:keys [guy1 guy2]} s]
    (q/stroke 0)
    (scene/draw scene-graph (:pos guy1))
    (scene/draw scene-graph (:pos guy2))))

(defn guy->rigidbody [guy]
  (let [{:keys [pos velocity]} guy]
    (p/->RigidBody
     1
     (p/->Vector2 (first pos) (second pos))
     (p/->Vector2 (first velocity) (second velocity)))))

(defn rigidbody->guy [rigidbody]
  {:pos (let [pos (:position rigidbody)]
          [(:x pos) (:y pos)])
   :velocity (let [v (:velocity rigidbody)]
               [(:x v) (:y v)])})

(defn update [state ticks]
  (let [{:keys [guy1 guy2]} state
        [r1 r2] [(guy->rigidbody guy1)
                 (guy->rigidbody guy2)]
        colliding (p/colliding? r1 r2)
        test (when colliding (println colliding))
        [r1 r2] (if colliding
                  (p/collide r1 r2)
                  [r1 r2])
        [guy1 guy2] (if colliding
                      [(rigidbody->guy r1)
                       (rigidbody->guy r2)]
                      [guy1 guy2])]
    {:guy1 (move guy1 ticks)
     :guy2 (move guy2 ticks)}))

(defn -main []
  (qcore/run-sketch {:title "Balls"
                     :setup #'setup
                     :draw #'draw
                     :size (:size params)}
                    {:on-key-press (k/gen-on-keypress keys-atom)
                     :on-key-release (k/gen-on-keyrelease keys-atom)}
                    (:update-fps params)
                    (fn [ticks] (swap! state-atom #(update % ticks)))
                    #(when (:q @keys-atom) true)))

;;(-main)

