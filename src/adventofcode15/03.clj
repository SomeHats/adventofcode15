(ns adventofcode15.03
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn visit-houses [directions]
  (loop [direction (first directions)
         remaining (rest directions)
         houses (transient (set []))
         x 0 y 0]
    (if (nil? direction)
      (persistent! (conj! houses [x y]))
      (recur (first remaining)
             (rest remaining)
             (conj! houses [x y])
             (cond (= ">" direction) (inc x)
                   (= "<" direction) (dec x)
                   :else x)
             (cond (= "^" direction) (inc y)
                   (= "v" direction) (dec y)
                   :else y)))))

(defn uninterleave
  [seq]
  [(take-nth 2 seq) (take-nth 2 (rest seq))])

(defpuzzle "Day 3: Perfectly Spherical Houses in a Vacuum"
  [input (str/split (ask "Directions! Gimme") #"")]
  {:single-santa (count (visit-houses input))
   :double-santa (let [[santa-input robo-santa-input] (uninterleave input)]
                   (count (set/union
                            (visit-houses santa-input)
                            (visit-houses robo-santa-input))))})
