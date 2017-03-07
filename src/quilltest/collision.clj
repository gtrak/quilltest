(ns quilltest.collision
  (:require [quilltest.physics :as p]))

(defn check-collisions
  "create a map of colliding nodes"
  [nodes]
  (let [combinations (for [x nodes
                           y nodes
                           ;; note, this implies ref semantics, not
                           ;; value, for equality, each game object
                           ;; should be unique
                           :when (not (identical? x y))]
                       [x y])
        collided (filter (fn [[a b]]
                           (p/colliding? (:rigid-body a)
                                         (:rigid-body b)))
                         combinations)]
    (into {} collided)))

(defn update-rigidbodies
  [nodes collided-pairs]
  ;; iterate over game objects, checking to see
  ;; if it's part of a collided pair, replacing it
  ;; with the updated values if it is
  (map
   (fn [node]
     (if-let [node2 (collided-pairs node)]
       ;; here
       (let [[rigidbody-a _] (p/collide (:rigid-body node)
                                        (:rigid-body node2))
             new-node (assoc node :rigid-body rigidbody-a)]
         new-node)
       node))
   nodes))

(defn update-collisions [nodes]
  (let [collided-pairs (check-collisions nodes)
        updated (update-rigidbodies nodes collided-pairs)]
    updated))
