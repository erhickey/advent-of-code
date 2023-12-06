(ns y2023.day6
  (:require
    [clojure.string :as s]))

(defn parse-input [input]
  (let [[line1 line2] (s/split-lines input)
        times (re-seq #"\d+" line1)
        distances (re-seq #"\d+" line2)]
    (zipmap (map read-string times) (map read-string distances))))

(defn parse-input-2 [input]
  (let [[line1 line2] (s/split-lines input)
        times (re-seq #"\d+" line1)
        distances (re-seq #"\d+" line2)]
    (map read-string [(apply str times) (apply str distances)])))

(defn get-distance [max-time time-held]
  (* time-held (- max-time time-held)))

(defn num-winning-times [[time distance]]
  (let [distances (map #(get-distance time %) (range 1 (+ 1 time)))
        winning (filter #(> % distance) distances)]
    (count winning)))

(defn solve [input]
  (let [pairs (parse-input input)
        pairs-2 (parse-input-2 input)
        part1 (apply * (map num-winning-times pairs))
        part2 (num-winning-times pairs-2)]
    [part1 part2]))
