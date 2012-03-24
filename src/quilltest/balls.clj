(ns quilltest.balls
  (:require [quilltest.core :as qcore]
            [quilltest.keys :as k]
            [quilltest.scene :as scene])
  (:require [quil.core :as q])
  (:gen-class))


(def params
  {:size [800 600]
   :fps 60
   :update-fps 600})

(def state-atom (atom {:pos [5 6]
                       :velocity [0 0]}))

(def scene-graph
  (atom (let [children [(scene/->GraphNode
                         nil
                         (fn [[x y]]
                           (q/ellipse (- x 20) y 20 20)))
                        #_(scene/->GraphNode
                           nil
                           (fn [[x y]]
                             (q/ellipse x (+ y 20) 20 20)))
                        (scene/->GraphNode
                         nil
                         (fn [[x y]]
                           (q/ellipse x (- y 20) 20 20)))]
              guy (scene/->GraphNode
                   children
                   (fn [[x y]]
                     (q/ellipse (+ x 20) y 20 20)))]
          guy)))


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

(def step 0.001)

(defn accelerate [vx vy]
  (let [pressed @keys-atom
        vx (if (pressed :a) (- vx step) vx)
        vx (if (pressed :d) (+ vx step) vx)
        vy (if (pressed :w) (- vy step) vy)
        vy (if (pressed :s) (+ vy step) vy)]
    [vx vy]))

(defn move [state]
  (let [[vx vy] (bounds-check state)
        [vx vy] (accelerate vx vy)
        [x y] (:pos state)]
    (assoc state
      :pos [(+ x vx) (+ y vy)]
      :velocity [vx vy])))

(defn setup []
  (q/frame-rate (:fps params))
  (q/background 255)
  (q/stroke-weight 2)
  (q/smooth))

(defn draw []
  (q/background 255)
  (let [[x y] (:pos @state-atom)]
    (q/stroke 0)
    ;(println x1 y1 x2 y2)
    ;(line x1 y1 x2 y2)
    (scene/draw @scene-graph [x y])))

(defn -main []
  (qcore/run-sketch {:title "Balls"
                     :setup #'setup
                     :draw #'draw
                     :size (:size params)}
                    {:on-key-press (k/gen-on-keypress keys-atom)
                     :on-key-release (k/gen-on-keyrelease keys-atom)}
                    600
                    (fn [] (swap! state-atom #(move %)))
                    #(when (:q @keys-atom) true)))

;(-main)