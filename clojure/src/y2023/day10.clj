(ns y2023.day10
  (:require
   [clojure.string :as s]
   [util.collection :refer [in?]]))

(def pipe-map
  {[\s \|] \s
   [\n \|] \n
   [\e \-] \e
   [\w \-] \w
   [\s \L] \e
   [\w \L] \n
   [\s \J] \w
   [\e \J] \n
   [\n \7] \w
   [\e \7] \s
   [\n \F] \e
   [\w \F] \s})

(def dir-map
  {\n [0 -1]
   \s [0 1]
   \e [1 0]
   \w [-1 0]})

(defn parse-line [line y]
  (let [cs (map (fn [x] [x y]) (range))]
    (zipmap cs line)))

(defn parse-grid [lines]
  (let [ys (range 0 (count lines))
        ls (zipmap lines ys)]
    (reduce conj {} (mapcat (fn [[line y]] (parse-line line y)) ls))))

(defn adjust-coord [[x y] [x-delta y-delta]]
  [(+ x x-delta) (+ y y-delta)])

(defn first-moves [grid start]
  (let [above-pipe (adjust-coord start [0 -1])
        below-pipe (adjust-coord start [0 1])
        left-pipe (adjust-coord start [-1 0])
        right-pipe (adjust-coord start [1 0])
        above (if (in? [\| \7 \F] (get grid above-pipe)) [above-pipe \n] nil)
        below (if (in? [\| \L \J] (get grid below-pipe)) [below-pipe \s] nil)
        left (if (in? [\- \L \F] (get grid left-pipe)) [left-pipe \w] nil)
        right (if (in? [\- \7 \J] (get grid right-pipe)) [right-pipe \e] nil)]
    (filter #(not= nil %) [above below left right])))

(defn do-move [grid coord heading]
  (let [pipe (get grid coord)
        heading' (get pipe-map [heading pipe])
        coord' (adjust-coord coord (get dir-map heading'))]
    [coord' heading']))

(defn get-loop [grid start [coord heading]]
  (map first
    (take-while
      (fn [[coord _]] (not= coord start))
      (iterate
        (fn [[coord heading]]
          (let [[coord' heading'] (do-move grid coord heading)]
            [coord' heading']))
        [coord heading]))))

(defn solve [input]
  (let [grid (parse-grid (s/split-lines input))
        start (first (keep #(when (= (val %) \S) (key %)) grid))
        loop (get-loop grid start (first (first-moves grid start)))
        part1 (/ (+ 1 (count loop)) 2)
        part2 ""]
    [part1 part2]))
