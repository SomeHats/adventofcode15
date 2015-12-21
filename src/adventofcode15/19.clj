(ns adventofcode15.19
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.set :refer [union]]))

(defn parse-input [input]
  (let [replacements (re-seq #"(\w+) => (\w+)" input)
        start (second (re-find #"\n(\w+)\n" input))]
    {:start start
     :replacements (map rest replacements)}))
     ;; :replacements (map #(vector (re-pattern (second %)) (last %)) replacements)}))

(defn step [start replacements]
  (->> replacements
       (map (fn [[pattern sub]]
              (let [pattern-length (count pattern)
                    start-length (count start)]
                (->> (range (count start))
                     (filter #(= pattern (subs start % (min start-length (+ % pattern-length)))))
                     (map #(str (subs start 0 %) sub (subs start (+ % pattern-length))))))))
       (flatten)
       (set)))

(defn build [target replacements]
  (let [replacements (map reverse replacements)]
    (loop [n 0
           molecules #{target}]
      ;; (println "\n=================\n")
      ;; (println "Step" n (count molecules))
      ;; (println (str/join "\n" (sort-by count molecules)))
      (if (or (contains? molecules "e") (< 1000 n))
        n
        (recur (inc n)
               (->> (map #(step % replacements) molecules)
                    (apply union)
                    ;; (filter #(>= 1 (count (re-seq #"e" %))))
                    (sort-by count)
                    (take 1)
                    (set)))))))

(defpuzzle "Day 19: Medicine for Rudolph"
  ;; [input (ask "Replacements and start molecule:" "file:dev-resources/day19-test.txt")]
  [input (ask "Replacements and start molecule:" "file:resources/day19.txt")]
  (let [{:keys [replacements start]} (parse-input input)]
    {:part-1 (count (step start replacements))
     :part-2 (build start replacements)}))
