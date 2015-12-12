(ns adventofcode15.12
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.data.json :as json]))

(defn flatten-all
  "Seq of all the values from a nested persistent data structure"
  ([data] (flatten-all (fn [_] false) data))
  ([ignore? data] (cond
                    (ignore? data) '()
                    (map? data)
                      (->> data
                           (vals)
                           (map #(flatten-all ignore? %1))
                           (flatten))
                    (coll? data)
                      (->> data
                           (map #(flatten-all ignore? %1))
                           (flatten))
                    :else data)))

(defpuzzle "Day 12: JSAbacusFramework.io"
  [json-input (ask "FEED MEH A JSON:" "file:resources/day12.txt")]
  (let [input (json/read-str json-input)]
    {:total (->> input
                 (flatten-all)
                 (filter number?)
                 (reduce + 0))
     :ignoring-red (->> input
                        (flatten-all #(and (map? %1) (some #{"red"} (vals %1))))
                        (filter number?)
                        (reduce + 0))}))

