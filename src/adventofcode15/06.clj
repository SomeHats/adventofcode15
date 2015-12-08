(ns adventofcode15.06
  (:gen-class)
  (:require [adventofcode15.util :refer :all]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [cartesian-product]]))

;; Part 1 actions:
(defn turn-on-1 [light] 1)
(defn turn-off-1 [light] 0)
(defn toggle-1 [light] (if (= light 1) 0 1))
(def actions-1 {"turn on" turn-on-1 "turn off" turn-off-1 "toggle" toggle-1})

;; Part 2 actions:
(defn turn-on-2 [light] (inc light))
(defn turn-off-2 [light] (max (dec light) 0))
(defn toggle-2 [light] (+ 2 light))
(def actions-2 {"turn on" turn-on-2 "turn off" turn-off-2 "toggle" toggle-2})

(def instruction-re #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)")

(defn parse-instruction
  "Parse a single instruction from a string"
  [instruction action-set]
  (let [[_ action-name x1 y1 x2 y2] (re-matches instruction-re instruction)]
    {:start [(read-string x1) (read-string y1)]
     :end [(read-string x2) (read-string y2)]
     :action (action-set action-name)}))

(defn parse-instructions
  "Parse a string of many instructions, 1 per line"
  [string action-set]
  (map #(parse-instruction % action-set) (str/split string #"\n")))

(defn get-lights
  "Create a starting array of lights"
  [width height]
  (let [row (vec (repeat width 0))]
    (vec (repeat height row))))

(defn rect
  "Get a seq of coordinates from a rectangle"
  [[x1 y1] [x2 y2]]
  (let [xs (range x1 (inc x2))
        ys (range y1 (inc y2))]
    (cartesian-product xs ys)))

(defn update-light!
  "Update a single light"
  [lights x y action]
  (let [row (get lights y)
        light (get row x)]
    (assoc! row x (action light))
    lights))

(defn apply-instruction!
  "Apply a single instruction to a set of lights"
  [lights {:keys [action start end]}]
  (reduce (fn [lights [x y]] (update-light! lights x y action))
          lights
          (rect start end)))

(defn apply-instructions
  "Apply a sequence of instructions to a set of lights"
  [instructions lights]
  (let [mut-lights (transient (vec (map transient lights)))]
    (->> (reduce apply-instruction! mut-lights instructions)
         (persistent!)
         (map persistent!))))

(defn print-lights
  [lights]
  (->> lights
       (map (fn [row] (str/join "" (map str row))))
       (str/join "\n")
       (println)))

(defpuzzle "Day 6: Probably a Fire Hazard"
  [width (read-string (ask "Light array width:"))
   height (read-string (ask "Light array height:"))
   is-part-2 (yes-no "Should I use the part 2 instruction set?")
   should-print (yes-no "Should I print the resulting light array")
   str-instructions (ask "Instructions:")]
  (let [lights (get-lights width height)
        instructions (parse-instructions str-instructions (if is-part-2 actions-2 actions-1))
        new-lights (apply-instructions instructions lights)]
    (when should-print (print-lights new-lights))
    (->> new-lights
         (flatten)
         (reduce +))))
