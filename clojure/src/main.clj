(ns main
  (:require
    [y2015.day1]
    [y2015.day5]
    [y2015.day6]
    [y2015.day7]
    [y2015.day8]
    [y2022.day1]
    [y2023.day1]
    [y2023.day2]
    [y2023.day3]
    [y2023.day4]
    [y2023.day5]
    [y2023.day6]
    [y2023.day7]
    [y2023.day8]))

(defn -main [day year inputFile]
  (println (str "\n" year " day " day))
  (println "---------------------------")
  (time (let [input (slurp inputFile)
        [part1 part2] ((ns-resolve (symbol (str "y" year ".day" day)) 'solve) input)]
        (println (str "part 1: " part1))
        (println (str "part 2: " part2))
        (println "---------------------------"))))
