(ns adventofcode15.23
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(def instruction-register #"(hlf|tpl|inc) (\w+)")
(def instruction-offset #"(jmp) ([+-]\d+)")
(def instruction-register-offset #"(ji[eo]) (\w+), ([+-]\d+)")

(defn parse-instruction [instruction]
  (let [ir (re-matches instruction-register instruction)
        io (re-matches instruction-offset instruction)
        iro (re-matches instruction-register-offset instruction)]
    (cond
      ir (let [[_ i r] ir] {:ins (keyword i) :reg (keyword r)})
      io (let [[_ i o] io] {:ins (keyword i) :offset (read-string o)})
      iro (let [[_ i r o] iro] {:ins (keyword i) :reg (keyword r) :offset (read-string o)})
      :else (throw (Exception. (str "Bad instruction: " instruction))))))

(defn parse-program [program]
  (->> (str/split program #"\n|;")
       (map str/trim)
       (map parse-instruction)
       (vec)))

(defn inc-pc [machine] (update machine :pc inc))

(defn step [{:keys [ins reg offset]} machine]
  (case ins
    :hlf (inc-pc (update machine reg #(int (/ % 2))))
    :tpl (inc-pc (update machine reg #(* 3 %)))
    :inc (inc-pc (update machine reg inc))
    :jmp (update machine :pc #(+ % offset))
    :jie (if (even? (reg machine))
           (update machine :pc #(+ % offset))
           (inc-pc machine))
    :jio (if (= 1 (reg machine))
           (update machine :pc #(+ % offset))
           (inc-pc machine))
    (throw (Exception. (str "Unknown instruction: " ins " " {:reg reg, :offset offset})))))

(defn run [program machine]
  (if (contains? program (:pc machine))
    (recur program (step (nth program (:pc machine)) machine))
    machine))

(defpuzzle "Day 23: Opening the Turing Lock"
  [program (ask "Program:" "file:resources/day23.txt")
   start-machine (read-string (ask "Starting Machine:" "{:a 0, :b 0, :pc 0}"))]
  (run (parse-program program) start-machine))
