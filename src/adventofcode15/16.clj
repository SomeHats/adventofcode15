(ns adventofcode15.16
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn parse-sue
  [sue]
  (if-let [[_ id prop-str] (re-matches #"(.*?): (.*)" sue)]
    {:name id
     :props (->> (re-seq #"(\w+): (\d+),?" prop-str)
                 (map #(vector (keyword (second %)) (read-string (last %))))
                 (into {}))}
    (throw (Exception. (str "Bad sue: " sue)))))

(defn parse-sues
  [string]
  (->> (str/split string #"\n|;")
       (map str/trim)
       (map parse-sue)
       (map #(vector (:name %) (:props %)))
       (into {})))

(defn compare-maps
  [a b compare-keys]
  (reduce
    (fn [score [key val]]
      (let [compare (key compare-keys =)]
        (cond (nil? (key b)) score
              (compare val (key b)) (inc score)
              :else score)))
    0 a))

(def default-sue
  "children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1")

(def part-2-rules
  {:cats <
   :trees <
   :pomeranians >
   :goldfish >})

(defpuzzle "Day 16: Aunt Sue"
  [sues (ask "Sues:" "file:resources/day16.txt")
   match-sue (ask "Sue to match:" default-sue)]
  (let [sues (parse-sues sues)
        match-sue (:props (parse-sue (str "default: " match-sue)))]
    {:part-1 (->> sues
                  (map #(vector (first %)
                                (compare-maps match-sue (second %) {})))
                  (sort-by second)
                  (filter #(= 3 (second %))))
     :part-2 (->> sues
                  (map #(vector (first %)
                                (compare-maps match-sue (second %) part-2-rules)))
                  (sort-by second)
                  (filter #(= 3 (second %))))}))
