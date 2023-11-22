(ns main
  (:require
    [y2015.day1]))

(defn -main [day year inputFile]
  (println (str "\n" year " day " day))
  (println "---------------------------")
  (let [input (slurp inputFile)
        [part1 part2] (time ((ns-resolve (symbol (str "y" year ".day" day)) 'solve) input))]
        (println "---------------------------")
        (println (str "part 1: " part1))
        (println (str "part 2: " part2))))
