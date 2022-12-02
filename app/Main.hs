module Main where

import qualified Data.Text.IO as TIO

import qualified DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)
import qualified DayTwo (personalHighScore, personalHighScore')

main :: IO ()
main = do
    dayOneInput <- TIO.readFile "input/DayOne.txt"
    putStrLn $ "Day One Part One: " ++ show (DayOne.highestCaloriesBrought dayOneInput)
    putStrLn $ "Day One Part Two: " ++ show (DayOne.topThreeHighestCaloriesBrought dayOneInput)
    dayTwoInput <- TIO.readFile "input/DayTwo.txt"
    putStrLn $ "Day Two Part One: " ++ show (DayTwo.personalHighScore dayTwoInput)
    putStrLn $ "Day Two Part Two: " ++ show (DayTwo.personalHighScore' dayTwoInput)