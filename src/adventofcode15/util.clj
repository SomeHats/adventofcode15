(ns adventofcode15.util
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defmacro defpuzzle
  "Define puzzles and solutions!"
  [title & solution]
  `(defn ~'-main ~title []
     (println ~(str "*** " title " ***"))
     (println "===========================")
     (let [~'result ~(conj solution 'do)]
       (println "===========================")
       (println "Result")
       (pprint ~'result))))

(defn ask
  "Ask the user for some input"
  [question]
  (print (str question " "))
  (flush)
  (let [input (read-line)]
    (if (re-find #"^file\:" input)
      (slurp (str/replace input #"^file\: ?" ""))
      input)))

(defn yes-no
  "Ask the user a yes-no question"
  [question]
  (let [answer (ask (str question " (Y/N)"))]
    (cond (re-matches #"(?i)^y|yes|t|true" answer) true
          (re-matches #"(?i)^n|no|f|false" answer) false
          :else (do (println "Please answer yes or no.") (yes-no question)))))
