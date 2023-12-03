(ns y2023.day3
  (:require
   [clojure.string :as s]
   [util.collection :refer [in?]]))

(defn line-group [lines ix]
  (let [above (if (= 0 ix) nil (nth lines (- ix 1)))
        below (if (= (+ ix 1) (count lines)) nil (nth lines (+ ix 1)))]
    (filter (partial not= nil) [above (nth lines ix) below])))

(defn rect-line [line ix l]
  (let [start (if (= ix 0) 0 (- ix 1))
        end (if (= (+ ix l) (count line)) (+ ix l) (+ ix l 1))]
    (subs line start end)))

(defn re-seq-pos-len [pattern string]
  (let [m (re-matcher pattern string)]
    ((fn step []
       (when (. m find)
         (cons [(. m group) (. m start) (- (. m end) (. m start))]
          (lazy-seq (step))))))))

(defn rectangles [lines ix]
  (let [line (nth lines ix)
        rect-lines (line-group lines ix)
        re-data (re-seq-pos-len #"\d+" line)]
    (map (fn [[n ix l]] [n (mapcat #(rect-line % ix l) rect-lines)]) re-data)))

(defn create-rectangles [lines]
  (mapcat (partial rectangles lines) (range 0 (count lines))))

(defn valid-rectangle [[_ rectangle]]
  (> (count (filter #(and (not= \. %) (not (Character/isDigit %))) rectangle)) 0))

(defn gear-ratio [gear-candidates ix]
  (let [valid-gears (filter (fn [[_ index len]] (in? (range (- index 1) (+ index len 1)) ix)) gear-candidates)]
    (if (= 2 (count valid-gears)) (apply * (map (fn [[n _ _]] (read-string n)) valid-gears)) 0)))

(defn gears [lines ix]
  (let [line (nth lines ix)
        gs (re-seq-pos-len #"\*" line)
        gear-lines (line-group lines ix)
        cns (mapcat (partial re-seq-pos-len #"\d+") gear-lines)]
    (map (partial gear-ratio cns) (map (fn [[_ ix _]] ix) gs))))

(defn create-gears [lines]
  (mapcat (partial gears lines) (range 0 (count lines))))

(defn solve [input]
  (let [lines (s/split-lines input)
        rs (create-rectangles lines)
        vrs (filter valid-rectangle rs)
        gs (create-gears lines)
        part1 (apply + (map read-string (map first vrs)))
        part2 (apply + gs)]
    [part1 part2]))
