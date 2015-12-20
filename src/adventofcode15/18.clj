(ns adventofcode15.18
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn parse-row
  [row]
  (vec (map #(= \# %) row)))

(defn parse-input
  [string]
  (->> (str/split string #"\n|;")
       (map str/trim)
       (map parse-row)
       (vec)))

(defn fmt
  [grid]
  (->> grid
       (map (fn [row]
              (map #(if % \# \.) row)))
       (map #(apply str %))
       (str/join "\n")))

(defn neighbours
  "Get's neighbours round a point in a 2D grid:
     543
     6*2
     701"
  [grid x y]
  [(get-in grid [(inc y) x])
   (get-in grid [(inc y) (inc x)])
   (get-in grid [y       (inc x)])
   (get-in grid [(dec y) (inc x)])
   (get-in grid [(dec y) x])
   (get-in grid [(dec y) (dec x)])
   (get-in grid [y       (dec x)])
   (get-in grid [(inc y) (dec x)])])

(defn map-2d
  [f grid]
  (vec (map (fn [row y] (vec (map #(f %1 [%2 y]) row (range))))
            grid
            (range))))

(defn step
  [original-grid]
  (map-2d (fn [val [x y]]
            (let [live-neighbours (->> (neighbours original-grid x y)
                                       (filter identity)
                                       (count))]
              (if val
                (or (= 2 live-neighbours) (= 3 live-neighbours))
                (= 3 live-neighbours))))
          original-grid))

(defn stick-corners
  [grid]
  (let [height (dec (count grid))
        width (dec (count (first grid)))]
    (-> grid
        (assoc-in [0 0] true)
        (assoc-in [height 0] true)
        (assoc-in [height width] true)
        (assoc-in [0 width] true))))

(defpuzzle "Day 18: Like a GIF For Your Yard"
  [input (ask "Starting state:" "file:resources/day18.txt")
   steps (read-string (ask "How many steps should I run for?" "100"))
   corners-stuck (yes-no "Are the corners stuck for part 2?" false)
   should-print (yes-no "Should I print the game?" false)]
  (let [grid (if corners-stuck
               (stick-corners (parse-input input))
               (parse-input input))
        game (iterate (if corners-stuck
                        (comp stick-corners step)
                        step) grid)]

    (when should-print (doall (->> (map #(do (println " - - - - - - - - - - - ")
                                             (println (fmt %))
                                             (Thread/sleep 50)) game)
                                   (take steps))))

    (->> (nth game steps)
         (flatten)
         (filter identity)
         (count))))
