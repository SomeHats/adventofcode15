(ns adventofcode15.09
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [permutations]]))

(defn parse-distance
  "Parse a single distance"
  [string]
  (if-let [[_ from to dist] (re-matches #"(\w+) to (\w+) = (\d+)" string)]
    {:from from
     :to to
     :dist (read-string dist)}
    (throw (Exception. (str "Bad distance string: " string)))))

(defn parse-distances
  "Parse a list of distances"
  [distances]
  (->> (str/split distances #"\n|;")
       (map str/trim)
       (map parse-distance)))

(defn add-distance-to-graph
  "Add a distance to a graph"
  ([graph {:keys [from to dist]}] (-> graph
                                      (add-distance-to-graph from to dist)
                                      (add-distance-to-graph to from dist)))
  ([graph a b dist] (if-let [node (get graph a)]
                      (assoc-in graph [a b] dist)
                      (assoc graph a {b dist}))))

(defn build-graph
  "Build a graph from a list of distances"
  [distances]
  (reduce add-distance-to-graph {} distances))

(defn get-distance
  "Find the total distance required to travel through a graph"
  [graph route]
  (->> route
       (partition 2 1)
       (reduce #(+ %1 (get-in graph %2)) 0)))

(defn find-routes
  "Find the distances of all possible routes through a graph"
  [graph]
  (->> (keys graph)
       (permutations)
       (map (fn [route] {:route route :dist (get-distance graph route)}))))

(defpuzzle "Day 9: All in a Single Night"
  [distances (ask "Distances:")]
  (let [routes (->> distances
                    (parse-distances)
                    (build-graph)
                    (find-routes)
                    (sort-by :dist))]
    {:shortest (first routes)
     :longest (last routes)}))
