(ns adventofcode15.05
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn old-nice?
  "Returns true if a string is nice, false if naughty"
  [string]
  (and (<= 3 (count (re-seq #"[aeiou]" string))) ; at least three vowels
       (re-find #"(.)\1" string) ; Repeated letter
       (not (re-find #"ab|cd|pq|xy" string)))) ; doesnt contain the naughty strings

(defn new-nice?
  "Is the string nice according to the new less completely ridiculous rules?"
  [string]
  (and (re-find #"(..).*\1" string) ; Pair of letters appearing twice
       (re-find #"(.).\1" string))) ; Letter repeating with another letter in between

(defpuzzle "Day 5: Doesn't He Have Intern-Elves For This?"
  [strings (str/split (ask "Gimme dem strings") #"\n")]
  {:old-rules (count (filter old-nice? strings))
   :new-rules (count (filter new-nice? strings))})
