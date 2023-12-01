(ns y2022.day1
  (:require
   [clojure.string :refer [split split-lines]]))

(defn solve [input]
  (let [bags (map split-lines (split input #"\n\n"))
        counts (map #(apply + (map read-string %)) bags)
        top3 (take 3 (reverse (sort counts)))
        part1 (apply max counts)
        part2 (apply + top3)]
    [part1 part2]))
