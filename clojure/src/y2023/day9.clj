(ns y2023.day9
  (:require
    [clojure.string :as s]))

(defn parse-line [line]
  (map read-string (re-seq #"-?\d+" line)))

(defn differences [ns]
  (last
    (reduce
      (fn [[prev acc] next] [next (conj acc (- next prev))])
      [(first ns) []]
      (rest ns))))

(defn extrapolate [history]
  (take-while
    #(not (every? (partial = 0) %))
    (iterate differences history)))

(defn find-next [history]
    (reduce
      (fn [acc xs] (+ acc (last xs)))
      0
      (reverse (extrapolate history))))

(defn solve [input]
  (let [histories (map parse-line (s/split-lines input))
        part1 (apply + (map find-next histories))
        part2 (apply + (map find-next (map reverse histories)))]
    [part1 part2]))
