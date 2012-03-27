(ns quilltest.physics)

;;; Simple functions to calculate physics interactions


(defprotocol Mathy
  "A mathiness protocol"
  (plus [a b])
  (minus [a b])
  (dot [a b])
  (norm [this] "Takes the euclidian norm"))

(extend-protocol Mathy
  java.lang.Number
  (plus [a b] (+ a b))
  (minus [a b] (- a b))
  (dot [a b] (* a b))
  (norm [this] (Math/sqrt (* this this))))

(defmacro with-math [& body]
  `(let [~'+ ~(fn [& args] (reduce plus args))
         ~'- ~(fn [& args] (reduce minus args))
         ~'* ~(fn [& args] (reduce dot args))]
     ~@body))

(defprotocol Vectory
  (normalize [this])
  (scalar* [this s])
  (scalar+ [this s]))

(declare ->Vector2)
(defrecord Vector2 [x y]
  Mathy
  (plus [a b] (let [new-x (+ (:x a) (:x b))
                    new-y (+ (:y a) (:y b))]
                (->Vector2 new-x new-y)))
  (minus [a b] (let [new-x (- (:x a) (:x b))
                     new-y (- (:y a) (:y b))]
                 (->Vector2 new-x new-y)))
  (dot [a b] (+ (* (:x a) (:x b)) (* (:y a) (:y b))))
  (norm [this] (Math/sqrt (dot this this)))
  Vectory
  (normalize [this]
    (let [magnitude (norm this)]      
      (scalar* this (/ 1 magnitude))))
  (scalar* [this a]
    (->Vector2 (* a (:x this)) (* a (:y this))))
  (scalar+ [this a]
    (->Vector2 (+ a (:x this)) (+ a (:y this)))))

(defrecord RigidBody 
    [mass position velocity])

(defn approaching [r1 r2]
  (let [v1 (:velocity r1)
        v2 (:velocity r2)]
    (with-math (neg? (* v1 v2)))))

(defn colliding? [r1 r2]
  (and (approaching r1 r2)
    (let [x1 (:position r1)
          x2 (:position r2)
          [x1 y1] [(:x x1) (:y x1)]
          [x2 y2] [(:x x2) (:y x2)]
          [dx dy] [(- x1 x2) (- y1 y2)]
          distance (Math/sqrt (+ (* dy dy) (* dx dx)))]
      (< distance 20))))

(defn collide
  "Elastically collides two rigid bodies, returning
   their new velocities"
  [r1 r2]
  (with-math
    (let [[x1 x2] [(:position r1) (:position r2)]
          normal (normalize (- x1 x2))
          tangent (->Vector2 (:y normal) (- 0 (:x normal)))
          {m1 :mass v1 :velocity} r1
          {m2 :mass v2 :velocity} r2
          M (+ m1 m2)
          [v1n v1t] [(scalar* normal (* v1 normal))
                     (scalar* tangent (* v1 tangent))]
          [v2n v2t] [(scalar* normal (* v2 normal))
                     (scalar* tangent (* v2 tangent))]
          [v1n_length v2n_length] [(norm v1n) (norm v2n)]
          v1 (scalar+ (+ v1t (scalar* normal (* (/ (- m1 m2) M) v1n_length)))
                      (* 2 (/ m2 M) v2n_length))
          v2 (scalar+ (+ v1t (scalar* normal (* (/ (- m1 m2) M) v2n_length)))
                      (* 2 (/ m1 M) v1n_length))
          body1 (->RigidBody m1 x1 v1)
          body2 (->RigidBody m2 x2 v2)]
      [body1 body2])))