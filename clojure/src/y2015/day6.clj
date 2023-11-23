(ns y2015.day6
  (:require
   [clojure.string :refer [split split-lines]]
   [util.collection :refer [in?]]))

(defn create-grid [default]
  (let [xs (range 0 1000)
        ys (range 0 1000)
        keys (mapcat #(map (fn [y] (str % ":" y)) ys) xs)]
    (zipmap keys (repeat default))))

(defn toggle [grid key]
  (assoc grid key (not (get grid key))))

(defn turn-on [grid key]
  (assoc grid key true))

(defn turn-off [grid key]
  (assoc grid key false))

(defn adjust-brightness [grid key val]
  (assoc grid key (max 0 (+ val (get grid key)))))

(defn parse-action-1 [s]
  (let [token (subs s 0 7)]
    (case token
          "toggle " toggle
          "turn on" turn-on
          "turn of" turn-off)))

(defn parse-action-2 [s]
  (let [token (subs s 0 7)]
    (case token
          "toggle " (fn [grid key] (adjust-brightness grid key 2))
          "turn on" (fn [grid key] (adjust-brightness grid key 1))
          "turn of" (fn [grid key] (adjust-brightness grid key -1)))))

(defn parse-coords [s]
  (let [[x1, y1, x2, y2] (map (fn [n] (Integer/parseInt n)) (mapcat (fn [w] (split w #",")) (filter #(in? (seq %) \,) (split s #" "))))
        xs (range x1 (+ x2 1))
        ys (range y1 (+ y2 1))]
    (mapcat #(map (fn [y] (str % ":" y)) ys) xs)))

(defn parse-instruction-1 [s]
  (list (parse-action-1 s) (parse-coords s)))

(defn parse-instruction-2 [s]
  (list (parse-action-2 s) (parse-coords s)))

(defn follow-instruction [grid instruction]
  (let [[action coords] instruction]
    (reduce (fn [g c] (action g c)) grid coords)))

(defn follow-instructions [grid instructions]
  (reduce (fn [g i] (follow-instruction g i)) grid instructions))

(defn solve [input]
  (let [grid-1 (create-grid false)
        grid-2 (create-grid 0)
        instructions-1 (map parse-instruction-1 (split-lines input))
        instructions-2 (map parse-instruction-2 (split-lines input))
        complete-grid-1 (follow-instructions grid-1 instructions-1)
        complete-grid-2 (follow-instructions grid-2 instructions-2)
        part1 (count (filter #(= true %) (vals complete-grid-1)))
        part2 (apply + (vals complete-grid-2))]
    [part1 part2]))
