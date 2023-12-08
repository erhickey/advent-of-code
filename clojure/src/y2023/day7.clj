(ns y2023.day7
  (:require
   [clojure.string :as s]
   [util.collection :refer [occurrences]]))

(def card-map
  {\A 13
   \K 12
   \Q 11
   \J 10
   \T 9
   \9 8
   \8 7
   \7 6
   \6 5
   \5 4
   \4 3
   \3 2
   \2 1})

(def wild-card-map
  (assoc card-map \J 0))

(def hand-map
  {[5] 7
   [1 4] 6
   [2 3] 5
   [1 1 3] 4
   [1 2 2] 3
   [1 1 1 2] 2
   [1 1 1 1 1] 1})

(def wild-hand-map
  {5 {[0] 7}
   4 {[1] 7}
   3 {[2] 7
      [1 1] 6}
   2 {[3] 7
      [1 2] 6
      [1 1 1] 4}
   1 {[4] 7
      [1 3] 6
      [2 2] 5
      [1 1 2] 4
      [1 1 1 1] 2}
   0 hand-map})

(defn parse-line [line]
  (let [[cards bid] (s/split line #" ")]
    [cards (read-string bid)]))

(defn count-cards [cards]
  (let [uniques (set cards)]
    (if (empty? cards)
      [0]
      (sort (map #(occurrences cards %) uniques)))))

(defn hand-rank [cards]
  (get hand-map (count-cards cards)))

(defn hand-rank-wilds [cards]
  (let [num-wild (count (filter #{\J} cards))
        non-wild (count-cards (filter #(not= \J %) cards))]
    (get (get wild-hand-map num-wild) non-wild)))

(defn compare-cards [card-map hand1 hand2 ix]
  (let [h1 (get card-map (nth hand1 ix))
        h2 (get card-map (nth hand2 ix))]
    (if (= h1 h2)
      (compare-cards card-map hand1 hand2 (+ 1 ix))
      (compare h1 h2))))

(defn compare-hands [hand-ranker card-map]
 (fn [[hand1 _] [hand2 _]]
  (let [hand-1-rank (hand-ranker hand1)
        hand-2-rank (hand-ranker hand2)]
    (if (= hand-1-rank hand-2-rank)
      (compare-cards card-map hand1 hand2 0)
      (compare hand-1-rank hand-2-rank)))))

(defn score-hand [[index acc] [_ bid]]
  [(+ 1 index) (+ acc (* index bid))])

(defn score-hands [hands]
  (last (reduce score-hand [1 0] hands)))

(defn solve [input]
  (let [lines (s/split-lines input)
        pairs (map parse-line lines)
        part1 (score-hands (sort (compare-hands hand-rank card-map) pairs))
        part2 (score-hands (sort (compare-hands hand-rank-wilds wild-card-map) pairs))]
    [part1 part2]))
