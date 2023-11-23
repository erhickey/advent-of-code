(ns y2015.day7 
  (:require
   [clojure.string :as str :refer [split split-lines]]
   [util.int :refer [is-int]]))

(defn short-and-operator [key1 value target-key circuit-map]
  (assoc circuit-map target-key (bit-and (get circuit-map key1) value)))

(defn and-operator [key1 key2 target-key circuit-map]
  (assoc circuit-map target-key (bit-and (get circuit-map key1) (get circuit-map key2))))

(defn or-operator [key1 key2 target-key circuit-map]
  (assoc circuit-map target-key (bit-or (get circuit-map key1) (get circuit-map key2))))

(defn not-operator [key1 target-key circuit-map]
  (assoc circuit-map target-key (bit-not (get circuit-map key1))))

(defn right-shift-operator [key1 target-key value circuit-map]
  (assoc circuit-map target-key (bit-shift-right (get circuit-map key1) value)))

(defn left-shift-operator [key1 target-key value circuit-map]
  (assoc circuit-map target-key (bit-shift-left (get circuit-map key1) value)))

(defn wires-ready [circuit-map keys]
  (every? #(contains? circuit-map %) keys))

(defn parse-and [s]
  (let [[value _ key1 _ target-key] (split s #" ")]
    (if (is-int value)
      [(fn [circuit-map] (short-and-operator key1 (Integer/parseInt value) target-key circuit-map)) (list key1)]
      [(fn [circuit-map] (and-operator key1 value target-key circuit-map)) (list key1 value)])))

(defn parse-or [s]
  (let [[key1 _ key2 _ target-key] (split s #" ")]
    [(fn [circuit-map] (or-operator key1 key2 target-key circuit-map)) (list key1 key2)]))

(defn parse-not [s]
  (let [[_ key1 _ target-key] (split s #" ")]
    [(fn [circuit-map] (not-operator key1 target-key circuit-map)) (list key1)]))

(defn parse-right-shift [s]
  (let [[key1 _ value _ target-key] (split s #" ")]
    [(fn [circuit-map] (right-shift-operator key1 target-key (Integer/parseInt value) circuit-map)) (list key1)]))

(defn parse-left-shift [s]
  (let [[key1 _ value _ target-key] (split s #" ")]
    [(fn [circuit-map] (left-shift-operator key1 target-key (Integer/parseInt value) circuit-map)) (list key1)]))

(defn parse-value [s]
  (let [[value _ target-key] (split s #" ")]
    (if (is-int value)
      [(fn [circuit-map] (assoc circuit-map target-key (Integer/parseInt value))) (list)]
      [(fn [circuit-map] (assoc circuit-map target-key (get circuit-map value))) (list value)]
    )))

(defn parse-line [s]
  (if (boolean (re-find #"AND" s)) (parse-and s)
    (if (boolean (re-find #"OR" s)) (parse-or s)
      (if (boolean (re-find #"LSHIFT" s)) (parse-left-shift s)
        (if (boolean (re-find #"RSHIFT" s)) (parse-right-shift s)
          (if (boolean (re-find #"NOT" s)) (parse-not s)
            (parse-value s)))))))

(defn ready-circuit [accumulator circuit]
  (let [[f keys] circuit
        [circuit-map circuits] accumulator]
    (if (wires-ready circuit-map keys) [(f circuit-map) circuits] [circuit-map (conj circuits circuit)])))

(defn ready-circuits [circuits circuit-map]
  (let [[_circuit-map _circuits] (reduce ready-circuit [circuit-map (list)] circuits)]
    (if (= 0 (count _circuits)) _circuit-map (ready-circuits _circuits _circuit-map))))

(defn solve [input]
       (let [circuits (map parse-line (split-lines input))
             signals (ready-circuits circuits {})
             part1 (get signals "a")
             input-2 (str/replace input #"\n\d* -> b\n" (str "\n" part1 " -> b\n"))
             circuits-2 (map parse-line (split-lines input-2))
             signals-2 (ready-circuits circuits-2 {})
             part2 (get signals-2 "a")]
         [part1 part2]))
