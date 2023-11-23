(ns y2015.day5
  (:require
   [clojure.string :refer [split-lines]]
   [util.collection :refer [in?]]))

(defn has-invalid-sequence [s]
  (not= nil (re-find #"(ab)|(cd)|(pq)|(xy)" s)))

(defn has-repeating-char [s]
  (not= nil (re-find #"(.)\1{1,}" s)))

(defn has-repeating-char-with-gap [s]
  (not= nil (re-find #"(.).\1{1,}" s)))

(defn has-repeating-dupe [s]
  (not= nil (re-find #"(..).*\1" s)))

(defn vowel-count [s]
  (count (filter #(in? [\a \e \i \o \u] %) (seq s))))

(defn is-nice-string [s]
    (and (not (has-invalid-sequence s)) (>= (vowel-count s) 3) (has-repeating-char s)))

(defn is-nice-string-2 [s]
    (and (has-repeating-dupe s) (has-repeating-char-with-gap s)))

(defn solve [input]
  (let [lines (split-lines input)
        part1 (count (filter is-nice-string lines))
        part2 (count (filter is-nice-string-2 lines))]
        [part1 part2]))
