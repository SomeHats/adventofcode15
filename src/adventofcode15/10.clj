(ns adventofcode15.10
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn look-and-say
  "Look-and-say a string n times"
  ([string n] (reduce (fn [string _] (look-and-say string)) string (range n)))
  ([string] (->> string
                 (partition-by identity)
                 (map #(str (count %1) (first %1)))
                 (str/join ""))))

(defpuzzle "Day 10: Elves Look, Elves Say"
  [input (ask "Starting digits:" "1113222113")
   iterations (read-string (ask "Iterations:" "40"))]
  (count (look-and-say input iterations)))
