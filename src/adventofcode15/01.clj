(ns adventofcode15.01
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn solve
  [instructions stop-at-basement]
  (loop [floor 0
         position 0
         current (first instructions)
         remaining (rest instructions)]
    (if (or (nil? current) (and stop-at-basement (> 0 floor)))
      {:floor floor :position position}
      (recur (cond (= "(" current) (inc floor)
                   (= ")" current) (dec floor)
                   :else floor)
             (inc position) (first remaining) (rest remaining)))))

(defpuzzle "Day 1: Not Quite Lisp"
  (let [input (str/split (ask "Enter the input:") #"")
        stop-at-basement (yes-no "Should I stop at the basement?")]
    (solve input stop-at-basement)))

