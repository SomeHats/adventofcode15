(ns adventofcode15.util
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defmacro defpuzzle
  "Define puzzles and solutions!"
  [title bindings & solution]
  `(defn ~'-main ~title []
     (println ~(str "*** " title " ***"))
     (println "===========================")
     (let ~bindings
       (println "===========================")
       (let [~'result (time (let [~'res ~(conj solution 'do)]
                              (if (seq? ~'res) (doall ~'res) ~'res)))]
         (println "===========================")
         (println "Result")
         (pprint ~'result)))))

(defn ask
  "Ask the user for some input"
  ([question] (ask question nil))
  ([question default]
    (print (if default (str question " (" default ") ") (str question " ")))
    (flush)
    (let [user-input (read-line)
          input (if (and default (= "" (str/trim user-input))) default user-input)]
      (if (re-find #"^file\:" input)
        (slurp (str/replace input #"^file\: ?" ""))
        input))))

(defn yes-no
  "Ask the user a yes-no question"
  ([question] (yes-no question nil))
  ([question default]
    (let [answer (ask (str question " (Y/N)") (case default
                                                true "Y"
                                                false "N"
                                                :else nil))]
      (cond (re-matches #"(?i)^y|yes|t|true" answer) true
            (re-matches #"(?i)^n|no|f|false" answer) false
            :else (do (println "Please answer yes or no.") (yes-no question))))))
