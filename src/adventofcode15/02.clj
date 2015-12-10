(ns adventofcode15.02
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn parse-box [box]
  (let [[_ & dimensions] (re-matches #"(\d+)x(\d+)x(\d+)" box)]
    (map read-string dimensions)))

(defn parse-list [input]
  (map parse-box (re-seq #"\d+x\d+x\d+" input)))

(defn paper-needed [[l w h]]
  (let [a (* l w)
        b (* w h)
        c (* h l)]
    (+ a a b b c c (min a b c))))

(defn required-paper [boxes]
  (reduce + 0 (map paper-needed boxes)))

(defn ribbon-needed [[l w h]]
  (let [[a b] (take 2 (sort [l w h]))
        perimeter (+ a a b b)
        volume (* l w h)]
    (+ perimeter volume)))

(defn required-ribbon [boxes]
  (reduce + 0 (map ribbon-needed boxes)))

(defpuzzle "Day 2: I Was Told There Would Be No Math"
  [input (parse-list (ask "Input plz." "file:resources/day2.txt"))]
  {:paper (required-paper input) :ribbon (required-ribbon input)})
