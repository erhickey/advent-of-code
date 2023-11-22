module Main where
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import System.Environment (getArgs)
import Text.Printf (printf)

import qualified Y2015.Day1 as Y2015_1
import qualified Y2015.Day2 as Y2015_2
import qualified Y2015.Day3 as Y2015_3
import qualified Y2015.Day4 as Y2015_4
import qualified Y2016.Day1 as Y2016_1
import qualified Y2016.Day2 as Y2016_2
import qualified Y2016.Day3 as Y2016_3
import qualified Y2016.Day4 as Y2016_4
import qualified Y2017.Day1 as Y2017_1
import qualified Y2017.Day2 as Y2017_2
import qualified Y2018.Day1 as Y2018_1
import qualified Y2018.Day2 as Y2018_2
import qualified Y2018.Day3 as Y2018_3
import qualified Y2018.Day4 as Y2018_4
import qualified Y2018.Day5 as Y2018_5
import qualified Y2018.Day6 as Y2018_6
import qualified Y2018.Day7 as Y2018_7
import qualified Y2018.Day8 as Y2018_8
import qualified Y2018.Day9 as Y2018_9
import qualified Y2018.Day10 as Y2018_10
import qualified Y2018.Day11 as Y2018_11
import qualified Y2018.Day12 as Y2018_12
import qualified Y2018.Day13 as Y2018_13
import qualified Y2018.Day14 as Y2018_14
import qualified Y2018.Day15 as Y2018_15
import qualified Y2019.Day1 as Y2019_1
import qualified Y2019.Day2 as Y2019_2
import qualified Y2019.Day3 as Y2019_3
import qualified Y2019.Day4 as Y2019_4
import qualified Y2019.Day5 as Y2019_5
import qualified Y2019.Day6 as Y2019_6
import qualified Y2019.Day7 as Y2019_7
import qualified Y2019.Day8 as Y2019_8
import qualified Y2019.Day9 as Y2019_9
import qualified Y2019.Day10 as Y2019_10
import qualified Y2020.Day1 as Y2020_1
import qualified Y2020.Day2 as Y2020_2
import qualified Y2020.Day3 as Y2020_3
import qualified Y2020.Day4 as Y2020_4
import qualified Y2020.Day5 as Y2020_5
import qualified Y2020.Day6 as Y2020_6
import qualified Y2020.Day7 as Y2020_7
import qualified Y2020.Day8 as Y2020_8
import qualified Y2020.Day9 as Y2020_9
import qualified Y2020.Day10 as Y2020_10
import qualified Y2020.Day11 as Y2020_11
import qualified Y2020.Day12 as Y2020_12
import qualified Y2020.Day13 as Y2020_13
import qualified Y2020.Day14 as Y2020_14
import qualified Y2020.Day15 as Y2020_15
import qualified Y2020.Day16 as Y2020_16
import qualified Y2020.Day17 as Y2020_17
import qualified Y2020.Day18 as Y2020_18
import qualified Y2020.Day19 as Y2020_19
import qualified Y2020.Day20 as Y2020_20
import qualified Y2020.Day21 as Y2020_21
import qualified Y2020.Day22 as Y2020_22
import qualified Y2021.Day1 as Y2021_1
import qualified Y2021.Day2 as Y2021_2
import qualified Y2021.Day3 as Y2021_3
import qualified Y2021.Day4 as Y2021_4
import qualified Y2021.Day5 as Y2021_5
import qualified Y2021.Day6 as Y2021_6
import qualified Y2021.Day7 as Y2021_7
import qualified Y2021.Day8 as Y2021_8
import qualified Y2021.Day9 as Y2021_9
import qualified Y2021.Day10 as Y2021_10
import qualified Y2021.Day11 as Y2021_11
import qualified Y2021.Day13 as Y2021_13
import qualified Y2021.Day14 as Y2021_14
import qualified Y2021.Day15 as Y2021_15
import qualified Y2021.Day16 as Y2021_16
import qualified Y2021.Day17 as Y2021_17

main :: IO ()
main = do
  [day, year, inputFile] <- getArgs
  input <- readFile inputFile
  let s = solver (read day) (read year)
  let (part1, part2) = s input
  printf "\n%s day %s\n" year day
  putStrLn "---------------------------"
  start <- getCurrentTime
  printf "part 1: %s\n" part1
  printf "part 2: %s\n" part2
  end <- getCurrentTime
  putStrLn "---------------------------"
  printf "run time: %ss" (show . nominalDiffTimeToSeconds $ diffUTCTime end start)

solver :: Int -> Int -> (String -> (String, String))
solver 1 2015 = Y2015_1.solve
solver 2 2015 = Y2015_2.solve
solver 3 2015 = Y2015_3.solve
solver 4 2015 = Y2015_4.solve
solver 1 2016 = Y2016_1.solve
solver 2 2016 = Y2016_2.solve
solver 3 2016 = Y2016_3.solve
solver 4 2016 = Y2016_4.solve
solver 1 2017 = Y2017_1.solve
solver 2 2017 = Y2017_2.solve
solver 1 2018 = Y2018_1.solve
solver 2 2018 = Y2018_2.solve
solver 3 2018 = Y2018_3.solve
solver 4 2018 = Y2018_4.solve
solver 5 2018 = Y2018_5.solve
solver 6 2018 = Y2018_6.solve
solver 7 2018 = Y2018_7.solve
solver 8 2018 = Y2018_8.solve
solver 9 2018 = Y2018_9.solve
solver 10 2018 = Y2018_10.solve
solver 11 2018 = Y2018_11.solve
solver 12 2018 = Y2018_12.solve
solver 13 2018 = Y2018_13.solve
solver 14 2018 = Y2018_14.solve
solver 15 2018 = Y2018_15.solve
solver 1 2019 = Y2019_1.solve
solver 2 2019 = Y2019_2.solve
solver 3 2019 = Y2019_3.solve
solver 4 2019 = Y2019_4.solve
solver 5 2019 = Y2019_5.solve
solver 6 2019 = Y2019_6.solve
solver 7 2019 = Y2019_7.solve
solver 8 2019 = Y2019_8.solve
solver 9 2019 = Y2019_9.solve
solver 10 2019 = Y2019_10.solve
solver 1 2020 = Y2020_1.solve
solver 2 2020 = Y2020_2.solve
solver 3 2020 = Y2020_3.solve
solver 4 2020 = Y2020_4.solve
solver 5 2020 = Y2020_5.solve
solver 6 2020 = Y2020_6.solve
solver 7 2020 = Y2020_7.solve
solver 8 2020 = Y2020_8.solve
solver 9 2020 = Y2020_9.solve
solver 10 2020 = Y2020_10.solve
solver 11 2020 = Y2020_11.solve
solver 12 2020 = Y2020_12.solve
solver 13 2020 = Y2020_13.solve
solver 14 2020 = Y2020_14.solve
solver 15 2020 = Y2020_15.solve
solver 16 2020 = Y2020_16.solve
solver 17 2020 = Y2020_17.solve
solver 18 2020 = Y2020_18.solve
solver 19 2020 = Y2020_19.solve
solver 20 2020 = Y2020_20.solve
solver 21 2020 = Y2020_21.solve
solver 22 2020 = Y2020_22.solve
solver 1 2021 = Y2021_1.solve
solver 2 2021 = Y2021_2.solve
solver 3 2021 = Y2021_3.solve
solver 4 2021 = Y2021_4.solve
solver 5 2021 = Y2021_5.solve
solver 6 2021 = Y2021_6.solve
solver 7 2021 = Y2021_7.solve
solver 8 2021 = Y2021_8.solve
solver 9 2021 = Y2021_9.solve
solver 10 2021 = Y2021_10.solve
solver 11 2021 = Y2021_11.solve
solver 13 2021 = Y2021_13.solve
solver 14 2021 = Y2021_14.solve
solver 15 2021 = Y2021_15.solve
solver 16 2021 = Y2021_16.solve
solver 17 2021 = Y2021_17.solve
solver day year = const ("solver not found for day " ++ show day, show year)
