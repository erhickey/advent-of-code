(ns y2015.day6
  (:require
   [clojure.string :refer [split-lines]]
   [instaparse.core :as insta]
   [util.vec2d :refer [grid-assoc grid-create grid-get grid-vals]]))

(defn adjust-brightness [value grid x y]
  (grid-assoc grid x y (max 0 (+ value (grid-get grid x y)))))

(defn create-action [f1 f2 x1 y1 x2 y2]
  (fn [part grid]
    (let [f (if (= part 1) f1 f2)
          xs (range x1 (+ x2 1))
          ys (range y1 (+ y2 1))
          coords (mapcat #(map (fn [y] [% y]) ys) xs)]
      (reduce (fn [acc [x y]] (f acc x y)) grid coords))))

(def parse-action
  (insta/parser
    "<action> = on | off | toggle
     on = <'turn on '> number <','> number <' through '> number <','> number
     off = <'turn off '> number <','> number <' through '> number <','> number
     toggle = <'toggle '> number <','> number <' through '> number <','> number
     number = #'[0-9]+'"))

(defn transform-action [tree]
  (insta/transform
    {:on (partial create-action #(grid-assoc %1 %2 %3 true) (partial adjust-brightness 1))
     :off (partial create-action #(grid-assoc %1 %2 %3 false) (partial adjust-brightness -1))
     :toggle (partial create-action #(grid-assoc %1 %2 %3 (not (grid-get %1 %2 %3))) (partial adjust-brightness 2))
     :number read-string} tree))

(defn run-actions [part actions]
  (reduce (fn [acc f] (f part acc)) (grid-create 1000 1000 (if (= part 1) false 0)) actions))

(defn solve [input]
  (let [actions (mapcat #(transform-action (parse-action %)) (split-lines input))
        part1 (count (filter (partial = true) (grid-vals (run-actions 1 actions))))
        part2 (apply + (grid-vals (run-actions 2 actions)))]
    [part1 part2]))
