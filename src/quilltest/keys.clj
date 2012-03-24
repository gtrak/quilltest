(ns quilltest.keys
  (:require [clojure.set :as set]))

;;; Functions for generating keymaps

;;; known keys, add used ones here
(def key->int
  {:w 87
   :a 65
   :s 83
   :d 68})

(def int->key
  (set/map-invert key->int))

(defn int-key-code [^java.awt.event.KeyEvent event]
  (.getKeyCode event))

(defn gen-on-keypress
  "Creates a function that updates an atom of currently pressed
   keys, the value of which will be a set of keywords."
  [keys-atom]
  (fn [event]
    (swap!
     keys-atom
     (fn [keys]
       (let [int-key (int-key-code event)]
         (when-let [key (int->key int-key)]
           (if (nil? keys) #{key} (conj keys key))))))))

(defn gen-on-keyrelease
  "Creates a function that updates an atom of currently pressed
   keys when one is released"
  [keys-atom]
  (fn [event]
    (swap!
     keys-atom
     (fn [keys]
       (disj keys (int->key (int-key-code event)))))))