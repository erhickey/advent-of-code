(ns y2015.day1
  (:require
    [util.collection :refer[index-of]]))

(defn traverse [acc c]
  (+ acc (if (= c \)) -1 1)))

(defn solve [input]
  (let [part1 (reduce traverse 0 (seq input))
        part2 (index-of -1 (reductions traverse 0 (seq input)))]
        [part1 part2]))
