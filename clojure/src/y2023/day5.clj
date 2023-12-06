(ns y2023.day5
  (:require
    [clojure.string :as s]))

(defn parse-seeds [line]
  (map read-string (re-seq #"\d+" line)))

(defn parse-seeds-2 [line]
  (let [singles (map read-string (re-seq #"\d+" line))
        pairs (partition 2 singles)]
    (map (fn [[s r]] {:start s :end (+ s (- r 1))}) pairs)))

(defn parse-map [line]
  (let [matches (re-seq #"\d+" line)
        [dest source-start r] (map read-string matches)
        source-end (+ source-start (- r 1))]
    {:start source-start :end source-end :dest dest}))

(defn parse-maps [section]
  (let [lines (rest (s/split-lines section))]
    (map parse-map lines)))

(defn map-has-source [seed m]
  (and (>= seed (:start m)) (<= seed (:end m))))

(defn get-destination [seed m]
  (let [diff (- seed (:start m))]
    (+ diff (:dest m))))

(defn find-destination [seed maps]
  (let [m (first (filter #(map-has-source seed %) maps))]
    (if m (get-destination seed m) seed)))

(defn get-location [maps seed]
  (reduce find-destination seed maps))

(defn update-dest-range [m seed-range]
  (let [in-range? (and
                    (<= (:start seed-range) (:end m))
                    (>= (:end seed-range) (:start m)))
        start-diff (- (:start seed-range) (:start m))
        end-diff (- (:end seed-range) (:start seed-range))
        new-start (+ (:dest m) start-diff)
        new-end (+ new-start end-diff)]
    (if in-range? {:start new-start :end new-end :is-new true} seed-range)))

(defn do-split [seed-range m]
  (let [start-1 (:start seed-range)
        end-1 (cond
                (or
                  (< (:end seed-range) (:start m))
                  (> (:start seed-range) (:end m)))
                    (:end seed-range)
                (< start-1 (:start m))
                  (- (:start m) 1)
                :else
                  (min (:end seed-range) (:end m)))
        start-2 (if
                  (< end-1 (:end seed-range))
                  (+ 1 end-1)
                  nil)
        end-2 (cond
                (= nil start-2)
                  nil
                (>= start-2 (:end m))
                  (:end seed-range)
                :else
                  (min (:end seed-range) (:end m)))
        start-3 (if
                  (and (not= end-2 nil) (not= end-2 (:end seed-range)))
                  (+ 1 end-2)
                  nil)
        end-3 (:end seed-range)
        range-2 (if (not= start-2 nil) {:start start-2 :end end-2} nil)
        range-3 (if (not= start-3 nil) {:start start-3 :end end-3} nil)
        new-ranges (filter #(not= nil %) [{:start start-1 :end end-1} range-2 range-3])]
    (if (:is-new seed-range) [seed-range] (map #(update-dest-range m %) new-ranges))))

(defn split-seeds [seeds m]
  (mapcat #(do-split % m) seeds))

(defn get-location-ranges [maps seeds]
  (reduce
    (fn [ss ms] (map
                  (fn [{:keys [start end]}] {:start start :end end})
                  (reduce split-seeds ss ms)))
    seeds
    maps))

(defn solve [input]
  (let [sections (s/split input #"\n\n")
        seeds (parse-seeds (first sections))
        seeds-2 (parse-seeds-2 (first sections))
        maps (map parse-maps (rest sections))
        part1 (apply min (map #(get-location maps %) seeds))
        part2 (apply min (map :start (get-location-ranges maps seeds-2)))]
    [part1 part2]))
