(ns adventofcode15.13
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [adventofcode15.09 :as travelling-salesman]
            [clojure.math.combinatorics :refer [permutations]]))

(def happiness-re
  #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+).")

(defn parse-happiness
  [string]
  (if-let [[_ who gain-loss amt target] (re-matches happiness-re string)]
    {:from who
     :to target
     :dist (if (= "gain" gain-loss)
             (read-string amt)
             (* -1 (read-string amt)))}
    (throw (Exception. (str "Bad input: " string)))))

(defn parse-happinesses
  [string]
  (->> (str/split string #"\n|;")
       (map str/trim)
       (map parse-happiness)))

(defn build-graph
  [happinesses]
  (reduce (fn [graph {:keys [from to dist]}]
               (travelling-salesman/add-distance-to-graph graph from to dist)) {} happinesses))

(defn get-happiness
  [graph plan]
  (let [looped-plan (conj plan (last plan))]
    (->> looped-plan
         (partition 2 1)
         (reduce (fn [total [a b]]
                   (+ total (get-in graph [a b] 0) (get-in graph [b a] 0))) 0))))

(defn find-seating-plans
  [graph]
  (->> (keys graph)
       (permutations)
       (travelling-salesman/reject-duplicate-routes)
       (map (fn [plan] {:plan plan :happiness (get-happiness graph plan)}))))

(defpuzzle "Day 13: Knights of the Dinner Table"
  [input (ask "Attendee Happiness:" "file:resources/day13.txt")]
  (let [graph (->> input
                (parse-happinesses)
                (build-graph))
        graph2 (assoc graph "Me" {})]
    {:without-me (->> graph
                      (find-seating-plans)
                      (sort-by :happiness)
                      (last))
     :with-me (->> graph2
                   (find-seating-plans)
                   (sort-by :happiness)
                   (last))}))
