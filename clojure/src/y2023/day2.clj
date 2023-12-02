(ns y2023.day2
  (:require
   [clojure.string :as s]))

(defn count-marbles [pattern s]
  (let [matches (re-seq (re-pattern (str "(\\d+) " pattern)) s)
        groups (map first matches)]
    (map read-string groups)))

(defn is-valid [s]
  (let [blues (count-marbles "blue" s)
        reds (count-marbles "red" s)
        greens (count-marbles "green" s)]
    (and (every? #(<= % 12) reds) (every? #(<= % 13) greens) (every? #(<= % 14) blues))))

(defn get-id [s]
  (read-string (re-find #"\d+" s)))

(defn power [s]
  (let [blues (count-marbles "blue" s)
        reds (count-marbles "red" s)
        greens (count-marbles "green" s)]
    (* (apply max blues) (apply max reds) (apply max greens))))

(defn solve [input]
  (let [lines (s/split-lines input)
        part1 (apply + (map get-id (filter is-valid lines)))
        part2 (apply + (map power lines))]
    [part1 part2]))
