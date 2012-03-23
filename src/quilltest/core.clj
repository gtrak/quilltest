(ns quilltest.core
  (:use quil.core
        [quil.helpers.drawing :only [line-join-points]]
        [quil.helpers.seqs :only [range-incl steps]]
        [quil.helpers.calc :only [mul-add]])
  (:gen-class))


(def params
  {:size [800 600]})

(def state-atom (atom {:pos [5 6]
                       :velocity [0 0]
                       :keys #{}}))

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

(def keydefs {:w 87
              :a 65
              :s 83
              :d 68})

(def step 0.02)

(defn accelerate [vx vy]
  (let [keys (:keys @state-atom)
        vx (if (keys (:a keydefs)) (- vx step) vx)
        vx (if (keys (:d keydefs)) (+ vx step) vx)
        vy (if (keys (:w keydefs)) (- vy step) vy)
        vy (if (keys (:s keydefs)) (+ vy step) vy)]
    [vx vy]))

(defn move [state]
  (let [[vx vy] (bounds-check state)
        [vx vy] (accelerate vx vy)
        [x y] (:pos state)]
    (assoc state
      :pos [(+ x vx) (+ y vy)]
      :velocity [vx vy])))

(defn setup []
  (background 255)
  (frame-rate 60)
  (stroke-weight 2)
  (smooth)
  (no-loop))

(defn draw []
  (background 255)
  (let [[x y] (:pos @state-atom)]
    (stroke 0)
    ;(println x1 y1 x2 y2)
    ;(line x1 y1 x2 y2)
    (ellipse x y 20 20)
    (no-loop)))

(defmacro f [& body]
  `(fn [] ~body))

(defn int-key-code [^java.awt.event.KeyEvent event]
  (.getKeyCode event))

(defn on-key-press [event] 
  (swap!
   state-atom
   (fn [state]
     (let [keys (:keys state)
           key  (int-key-code event)
           keys (if (nil? keys) #{key} (conj keys key))]
;       (println (str "Pressed " key))
       (assoc state :keys keys)))))

(defn on-key-release [event]
  (swap!
   state-atom
   (fn [state]
     (let [keys (:keys state)
           keys (disj keys (int-key-code event))]
;       (println (str "Released " (int-key-code event)))
       (assoc state :keys keys)))))

(defmacro with-applet [& body]
  `(binding [quil.dynamics/*applet* mysketch]
    (do ~@body)))


(defn -main []
  (defsketch mysketch
    :title "Accelerate your balls"
    :setup setup
    :draw #'draw
    :size (:size params))
  (.addKeyListener
   mysketch
   (reify java.awt.event.KeyListener
     (keyPressed [this event] (on-key-press event))
     (keyReleased [this event] (on-key-release event))
     (keyTyped [this event] nil)))
  (with-applet
    (doseq [x (range)]
      (swap! state-atom #(move %))
      (start-loop)
;      (println state-atom)
      (Thread/sleep (/ 1000 120)))))










