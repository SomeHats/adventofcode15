(ns adventofcode15.07
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]))

(def forward-re #"\w+")
(def not-re #"NOT (\w+)")
(def bin-op-re #"(\w+) (AND|OR) (\w+)")
(def shift-op-re #"(\w+) (RSHIFT|LSHIFT) (\d+)")

(defn parse-input
  "Parse the lhs input of an instruction"
  [input]
  (cond
    (re-matches forward-re input) {:type :FORWARD :value input}
    (re-matches not-re input) {:type :NOT :rhs (get (re-matches not-re input) 1)}
    (re-matches bin-op-re input) (let [[_ lhs op rhs] (re-matches bin-op-re input)]
                                   {:type (keyword op) :lhs lhs :rhs rhs})
    (re-matches shift-op-re input) (let [[_ lhs op rhs] (re-matches shift-op-re input)]
                                     {:type (keyword op) :lhs lhs :rhs rhs})
    :else (throw (Exception. (str "Cannot parse lhs " input)))))

(defn parse-instruction
  "Parse a single instruction"
  [instruction]
  (let [[_ input-str target] (re-matches #"(.*) -> (\w+)" instruction)
        input (parse-input input-str)]
    [target input]))

(defn parse-instructions
  "Parse a string of instructions"
  [string]
  (->> (str/split string #"\n|;")
       (map str/trim)
       (sort)
       (map parse-instruction)))

(defn pad [amt val seq]
  (let [to-pad (- amt (count seq))]
    (if (> to-pad 0)
      (recur amt val (cons val seq))
      (vec seq))))

(def blank (vec (repeat 16 false)))

(defn b-not [signal] (map not signal))
(defn b-and [a b] (map #(and %1 %2) a b))
(defn b-or [a b] (map #(or %1 %2) a b))
(defn b-lshift [s amt] (into (vec (drop amt s)) (repeat amt false)))
(defn b-rshift [s amt] (into (take (- 16 amt) s) (repeat amt false)))

(defn b->int [signal]
  (->> signal
       (reverse)
       (map (fn [i bit] (if bit [i 1] [i 0])) (range (count signal)))
       (reduce (fn [acc [i bit]] (+ acc (* bit (int (Math/pow 2 i))))) 0)))

(defn int->b [i]
  (->> (Integer/toString i 2)
       (vec)
       (map #(if (= %1 \1) true false))
       (pad 16 false)))

(defn lookup!
  "find and calculate a value in the circuit, using a transient copy of the circuit for caching"
  [sym circuit]
  (if (re-matches #"\d+" sym)
    (int->b (read-string sym))
    (if-let [node (get circuit sym false)]
      ;; ((get ops (:type node) op-not-found) circuit node)
      (let [val (case (:type node)
                  :CACHED (:val node)
                  :NOT (let [{:keys [rhs]} node] (b-not (lookup! rhs circuit)))
                  :AND (let [{:keys [lhs rhs]} node] (b-and (lookup! lhs circuit) (lookup! rhs circuit)))
                  :OR (let [{:keys [lhs rhs]} node] (b-or (lookup! lhs circuit) (lookup! rhs circuit)))
                  :LSHIFT (let [{:keys [lhs rhs]} node] (b-lshift (lookup! lhs circuit) (b->int (lookup! rhs circuit))))
                  :RSHIFT (let [{:keys [lhs rhs]} node] (b-rshift (lookup! lhs circuit) (b->int (lookup! rhs circuit))))
                  :FORWARD (let [{:keys [value]} node] (lookup! value circuit)))]
        (assoc! circuit sym {:type :CACHED :val val})
        val)
      (throw (.Exception (str "Cannot find node " sym))))))

(defn lookup
  "Find and calculate a value in the circuit"
  [sym circuit]
  (lookup! sym (transient circuit)))

(defn build-circuit
  "Construct a graph from an instruction set"
  [instructions]
  (into {} instructions))

(defpuzzle "Day 7: Some Assembly Required"
  [instructions (ask "Instructions:")
   lookup-node (ask "Node to read value from:")
   override-node (ask "Node to override:")]
  (let [circuit (build-circuit (parse-instructions instructions))
        part-1 (lookup lookup-node circuit)
        overridden-circuit (assoc circuit override-node {:type :CACHED :val part-1})
        part-2 (lookup lookup-node overridden-circuit)]
    {:part-1 (b->int part-1) :part-2 (b->int part-2)}))
