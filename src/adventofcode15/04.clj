(ns adventofcode15.04
  (:gen-class)
  (:require [adventofcode15.util :refer :all]))

(defn md5
  [input]
  (apply str
    (map (partial format "%02x")
      (.digest (doto
        (java.security.MessageDigest/getInstance "MD5")
        .reset
        (.update (.getBytes input)))))))

(defn solve
  [input pattern]
  (->> (range)
       (map (fn [number] [number (md5 (str input number))]))
       (filter (fn [[_ md5-hash]] (re-find pattern md5-hash)))
       (first)))

(defpuzzle "Day 4: The Ideal Stocking Stuffer"
  (let [input (ask "Secret key:")
        pattern (re-pattern (ask "Pattern:"))]
    (solve input pattern)))
