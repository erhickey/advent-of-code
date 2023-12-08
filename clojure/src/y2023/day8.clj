(ns y2023.day8
  (:require
   [clojure.string :as s]
   [util.collection :refer [in?]]
   [util.math :refer [lcm]]))

(defn parse-map [line]
  (let [[k left right] (re-seq #"\w+" line)]
    {k [left right]}))

(defn do-step [ms current step]
  (let [[l r] (get ms current)]
    (if (= step \R) r l)))

(defn count-steps [ms steps start ends]
  (count
    (take-while
      #(not (in? ends %))
      (reductions
        (fn [pos step] (do-step ms pos step))
        start
        (cycle (seq steps))))))

(defn ghost-steps [ms steps]
  (let [starts (filter #(s/ends-with? % "A") (keys ms))
        ends (filter #(s/ends-with? % "Z") (keys ms))
        ss (map #(count-steps ms steps % ends) starts)]
    (reduce lcm ss)))

(defn solve [input]
  (let [[steps maps] (s/split input #"\n\n")
        ms (reduce conj {} (mapcat parse-map (s/split-lines maps)))
        part1 (count-steps ms steps "AAA" ["ZZZ"])
        part2 (ghost-steps ms steps)]
    [part1 part2]))
