(ns y2015.day8
  (:require
   [clojure.string :as str :refer [split-lines]]))

(defn escape [s]
  (str/replace
    (str/replace
      (str/replace
        (str/replace
          (str/replace
            s
            #"^\"" "")
          #"\"$" "")
        #"\\\\" "~")
      #"\\x.." "-")
    #"\\\"" "\""))

(defn encode [s]
  (reduce #(if (or (= %2 \\) (= %2 \")) (+ %1 2) (+ %1 1)) 2 s))

(defn solve [input]
  (let [lines (split-lines input)
        char-count (apply + (map count lines))
        mem-count (apply + (map count (map escape lines)))
        part1 (- char-count mem-count)
        part2 (- (apply + (map encode lines)) char-count)]
    [part1 part2]))
