module Main where

import qualified Data.Text.IO as TIO

import qualified DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)

main :: IO ()
main = do
    dayOneInput <- TIO.readFile "input/DayOne.txt"
    putStrLn $ "Day One Part One: " ++ show (DayOne.highestCaloriesBrought dayOneInput)
    putStrLn $ "Day One Part Two: " ++ show (DayOne.topThreeHighestCaloriesBrought dayOneInput)