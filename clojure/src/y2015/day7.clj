(ns y2015.day7
  (:require
   [clojure.string :as str :refer [split-lines]]
   [instaparse.core :as insta]
   [util.map :refer [contains-keys]]))

(defn create-instruction [f val1 val2 target]
  [#(assoc % target (f (get % val1 val1) (get % val2 val2)))
   (filter string? (list val1 val2))])

(def parse-instruction
  (insta/parser
    "<instruction> = and | or | not | lshift | rshift | direct
     and = (wire | signal) <' AND '> wire <arrow> wire
     or = wire <' OR '> wire <arrow> wire
     not = <'NOT '> wire <arrow> wire
     lshift = wire <' LSHIFT '> signal <arrow> wire
     rshift = wire <' RSHIFT '> signal <arrow> wire
     direct = (wire | signal) <arrow> wire
     arrow = ' -> '
     wire = #'[a-zA-Z]+'
     signal = #'[0-9]+'"))

(defn transform-instruction [tree]
  (insta/transform
    {:and (partial create-instruction bit-and)
     :or (partial create-instruction bit-or)
     :not (partial create-instruction (fn [_ val2] (bit-not val2)) 0)
     :lshift (partial create-instruction bit-shift-left)
     :rshift (partial create-instruction bit-shift-right)
     :direct (partial create-instruction (fn [_ val2] val2) 0)
     :signal read-string
     :wire identity} tree))

(defn run-instruction [[circuit-map instructions] [f deps :as instruction]]
  (if (contains-keys circuit-map deps)
    [(f circuit-map) instructions]
    [circuit-map (conj instructions instruction)]))

(defn run-instructions
  ([instructions] (run-instructions instructions {}))
  ([instructions circuit-map]
    (let [[_circuit-map _instructions] (reduce run-instruction [circuit-map (list)] instructions)]
      (if (= 0 (count _instructions)) _circuit-map (run-instructions _instructions _circuit-map)))))

(defn solve [input]
       (let [instructions (mapcat #(transform-instruction (parse-instruction %)) (split-lines input))
             part1 (get (run-instructions instructions) "a")
             input-2 (str/replace input #"\n\d* -> b\n" (str "\n" part1 " -> b\n"))
             instructions-2 (mapcat #(transform-instruction (parse-instruction %)) (split-lines input-2))
             part2 (get (run-instructions instructions-2) "a")]
         [part1 part2]))
