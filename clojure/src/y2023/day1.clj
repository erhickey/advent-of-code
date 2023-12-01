(ns y2023.day1
  (:require
   [clojure.string :as s :refer [split-lines]]))

(defn to-num [line]
  (let [ns (filter #(Character/isDigit %) line)]
  (read-string (str (first ns) (last ns)))))

(def num-map
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def pattern "1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine")

(defn to-num-2 [line]
  (let [first-match (re-find (re-pattern pattern) line)
        last-match (s/reverse (re-find (re-pattern (s/reverse pattern)) (s/reverse line)))]
    (read-string (str (num-map first-match first-match) (num-map last-match last-match)))))

(defn solve [input]
  (let [lines (split-lines input)
        part1 (apply + (map to-num lines))
        part2 (apply + (map to-num-2 lines))]
    [part1 part2]))
