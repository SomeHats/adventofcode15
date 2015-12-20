(ns adventofcode15.17
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parse-sizes
  [input]
  (->> (str/split input #"\n|;")
       (map str/trim)
       (map read-string)))

(def tag-id (atom 0))
(defn tag
  "Return a vector  of the value with a unique id: [id val]"
  [value]
  [(swap! tag-id inc) value])

(def untag second)

(defpuzzle "Day 17: No Such Thing as Too Much"
  [sizes (ask "Container sizes:" "file:resources/day17.txt")
   target (read-string (ask "Target amount of eggnog:" "150"))]
  (let [container-combos (->> (parse-sizes sizes)
                              (map tag)
                              (combo/subsets)
                              (map #(map untag %))
                              (filter #(= target (reduce + 0 %))))]
    {:part-1 (count container-combos)
     :part-2 (->> container-combos
                  (group-by count) ; group together the combos by number of containers
                  (sort-by first) ; Order the groups by number of containers
                  (first) ; Pick the group with the smallest number of containers
                  (last) ; Get just the combinations
                  (count))}))
