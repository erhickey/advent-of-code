(ns y2023.day4
  (:require
   [clojure.string :as s]
   [util.collection :refer [in?]]))

(defn parse-card [line]
  (let [[left right] (s/split line #"\|")
        winning (re-seq #"\d+" left)
        have (re-seq #"\d+" right)]
    [(rest winning) have]))

(defn count-matches [[winning have]]
  (count (filter #(in? winning %) have)))

(defn score-matches [num-matches]
  (int (Math/floor (Math/pow 2 (- num-matches 1)))))

(defn add-duplicates [[in out] n]
  (let [next-card (first in)
        out' (conj out next-card)
        dupes (concat (repeat n next-card) (repeat (count in) 0))
        in' (map + (drop 1 in) dupes)]
    [in' out']))

(defn duplicate-cards [nums-matches]
  (let [cards (repeat (count nums-matches) 1)]
    (last (reduce add-duplicates [cards []] nums-matches))))

(defn solve [input]
  (let [lines (s/split-lines input)
        cards (map parse-card lines)
        num-matches (map count-matches cards)
        duplicated (duplicate-cards num-matches)
        part1 (apply + (map score-matches num-matches))
        part2 (apply + duplicated)]
    [part1 part2]))
