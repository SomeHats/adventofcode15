(ns adventofcode15.11
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [expt floor]]))

(def alphabet (vec "abcdefghijklmnopqrstuvwxyz"))

(defn log
  [n base]
  (/ (Math/log n) (Math/log base)))

(defn word->int
  "Convert a word to int using digits"
  [digits word]
  (let [base (count digits)
        word (reverse word)]
    (loop [n 0
           place 0
           digit (first word)
           word (rest word)]
      (if digit
        (recur (+ n (* (.indexOf digits digit) (expt base place)))
               (inc place)
               (first word)
               (rest word))
        n))))

(defn int->word
  [digits n]
  (if (= 0 n)
    (str (first digits))
    (let [base (count digits)
          size (int (floor (log n base)))]
      (loop [word ""
             place size
             n n]
        (if (> 0 place)
          word
          (let [place-size (expt base place)
                digit (floor (/ n place-size))]
            (recur (str word (nth digits digit))
                   (dec place)
                   (- n (* place-size digit)))))))))

(def runs-re (->> alphabet
                  (partition 3 1)
                  (map str/join)
                  (str/join "|")
                  (re-pattern)))

(defn valid?
  [password]
  (and (re-find runs-re password)
       (not (re-find #"i|o|l" password))
       (<= 2 (count (distinct (re-seq #"(\w)\1" password))))))

(defn passwords
  "Return a lazy seq of passwords, incrementing according to alphabet"
  [password]
  (let [pw-int (word->int alphabet password)]
    (->> (iterate inc pw-int)
         (map #(int->word alphabet %1))
         (filter valid?))))

(defpuzzle "Day 11: Corporate Policy"
  [password (ask "Starting Password:" "vzbxkghb")
   n (read-string (ask "Passwords to generate:" "2"))]
  (take n (passwords password)))
