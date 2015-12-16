(ns adventofcode15.14
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn parse-reindeer
  [string]
  (->> (str/split string #"\n|;")
       (map #(re-matches #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds." %1))
       (map (fn [[_ name speed fly-duration rest-duration]]
              {:name name
               :speed (read-string speed)
               :fly-duration (read-string fly-duration)
               :rest-duration (read-string rest-duration)}))))

(defn step
  [{:keys [speed distance fly-timer fly-duration rest-timer rest-duration] :as deer}]
  (cond
    (< 0 fly-timer) (conj deer {:fly-timer (dec fly-timer)
                                :distance (+ distance speed)})
    (= 0 fly-timer) (do
                      (println (str (:name deer) " NAPS " rest-duration "s"))
                      (conj deer {:fly-timer -1
                                  :rest-timer rest-duration}))
    (< 1 rest-timer) (conj deer {:rest-timer (dec rest-timer)})
    (= 1 rest-timer) (do
                       (println (str (:name deer) " RUNS " fly-duration "s"))
                       (conj deer {:rest-timer -1
                                   :fly-timer fly-duration}))
    :else (throw (Exception. (str "Bad deer state: " deer)))))

(defn race
  [reindeer]
  (->> reindeer
       (map (fn [{:keys [fly-duration] :as deer}]
              (conj deer {:fly-timer fly-duration
                          :rest-timer 0
                          :distance 0})))
       (iterate #(map step %1))))

(defpuzzle "Day 14: Reindeer Olympics"
  [reindeer (ask "Reindeer:" "file:resources/day14.txt")
   race-duration (read-string (ask "Race duration:" "2503"))]
  (let [reindeer-race (race (parse-reindeer reindeer))]
    (->> (nth reindeer-race race-duration)
         (sort-by :distance))))
