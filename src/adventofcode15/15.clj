(ns adventofcode15.15
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(def ingredient-re #"(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)")

(defn parse-ingredient
  [string]
  (if-let [[_ name capacity durability flavor texture calories] (re-matches ingredient-re string)]
    {:name name
     :capacity (read-string capacity)
     :durability (read-string durability)
     :flavor (read-string flavor)
     :texture (read-string texture)
     :calories (read-string calories)}
    (throw (Exception. (str "Bad ingredient: " string)))))

(defn combinations
  [budget ingredients]
  (cond (= 1 (count ingredients))
        (seq {(first ingredients) budget})
        (< 1 (count ingredients))
        (let [ingredient (first ingredients)
              remaining (rest ingredients)]
          (mapcat (fn [amt]
                    (map #(conj {ingredient amt} %1) (combinations (- budget amt) remaining)))
                  (range (inc budget))))))

(defn parse-ingredients
  [string]
  (->> (str/split string #"\n|;")
       (map str/trim)
       (map parse-ingredient)))

(defn score-prop
  [prop combo]
  (let [s (->> combo
            (map (fn [[ingredient amount]]
                   (* amount (prop ingredient))))
            (apply +))]
    (if (> 0 s) 0 s)))

(defn score
  [combo]
  (->> [:capacity :durability :flavor :texture]
       (map #(score-prop % combo))
       (reduce *)))

(defn best-combo
  [combos]
  (->> (map #(vector % (score %)) combos)
       (sort-by last)
       (last)))

(defpuzzle "Day 15: Science for Hungry People"
  [ingredients (ask "Ingredients:" "file:resources/day15.txt")
   spoons (read-string (ask "Available teaspoons:" "100"))
   cal-budget (read-string (ask "Target calories:" "500"))]
  (let [combos (->> ingredients
                    (parse-ingredients)
                    (combinations spoons))]
    {:best-recipe (best-combo combos)
     :limited-calories (->> combos
                            (filter #(= cal-budget (score-prop :calories %)))
                            (best-combo))}))
