(ns adventofcode15.08
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(defn string-decode
  [string]
  (if (re-matches #"^\"(.*)\"$" string)
    (-> string
        (str/replace #"^\"|\"$" "")
        (str/replace #"\\\"" "\"")
        (str/replace "\\\\" "\\")
        (str/replace #"\\x[0-9a-f]{2}" "?"))
    (throw (Exception. (str "Badly formed string: " string)))))

(defn string-encode
  [string]
  (str "\"" (str/replace string #"(\"|\\)" "\\\\$1") "\""))

(defpuzzle "Day 8: Matchsticks"
  [strings (str/split (ask "Gimme dem strings") #"\n")]
  (let [code-size (->> strings
                       (map count)
                       (reduce +))
        mem-size (->> strings
                      (map string-decode)
                      (map count)
                      (reduce +))
        encoded-size (->> strings
                          (map string-encode)
                          (map count)
                          (reduce +))]
    {:decoded (- code-size mem-size)
     :encoded (- encoded-size code-size)}))
