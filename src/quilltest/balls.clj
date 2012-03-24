(ns quilltest.balls
  (:require [quilltest.core :as qcore]
            [quilltest.keys :as k])
  (:require [quil.core :as q])
  (:gen-class))


(def params
  {:size [800 600]
   :fps 50
   :update-fps 600})

(def state-atom (atom {:pos [5 6]
                       :velocity [0 0]}))

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
  (let [keys @keys-atom
        vx (if (keys :a) (- vx step) vx)
        vx (if (keys :d) (+ vx step) vx)
        vy (if (keys :w) (- vy step) vy)
        vy (if (keys :s) (+ vy step) vy)]
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
    (q/ellipse x y 20 20)))

(defn -main []
  (qcore/run-sketch {:title "Balls"
                     :setup setup
                     :draw draw
                     :size (:size params)}
                    {:on-key-press (k/gen-on-keypress keys-atom)
                     :on-key-release (k/gen-on-keyrelease keys-atom)}
                    600
                    (fn [] (swap! state-atom #(move %)))))

;(-main)