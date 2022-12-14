module Main where

import qualified Data.Text.IO as TIO

import qualified DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)
import qualified DayTwo (personalHighScore, personalHighScore')
import qualified DayThree (totalRucksackOrderPriority, totalBadgePriority)
import qualified DayFour (totalAssignmentOverlaps, totalAssignmentOverlaps')
import qualified DayFive (resultOfRearrangement, resultOfRearrangement')
import qualified DaySix (findStartOfPacketMarker, findStartOfMessageMarker)

main :: IO ()
main = do
    dayOneInput <- TIO.readFile "input/DayOne.txt"
    putStrLn $ "Day One Part One: " ++ show (DayOne.highestCaloriesBrought dayOneInput)
    putStrLn $ "Day One Part Two: " ++ show (DayOne.topThreeHighestCaloriesBrought dayOneInput)
    dayTwoInput <- TIO.readFile "input/DayTwo.txt"
    putStrLn $ "Day Two Part One: " ++ show (DayTwo.personalHighScore dayTwoInput)
    putStrLn $ "Day Two Part Two: " ++ show (DayTwo.personalHighScore' dayTwoInput)
    dayThreeInput <- TIO.readFile "input/DayThree.txt"
    putStrLn $ "Day Three Part One: " ++ show (DayThree.totalRucksackOrderPriority dayThreeInput)
    putStrLn $ "Day Three Part Two: " ++ show (DayThree.totalBadgePriority dayThreeInput)
    dayFourInput <- TIO.readFile "input/DayFour.txt"
    putStrLn $ "Day Four Part One: " ++ show (DayFour.totalAssignmentOverlaps dayFourInput)
    putStrLn $ "Day Foud Part Two: " ++ show (DayFour.totalAssignmentOverlaps' dayFourInput)
    dayFiveInput <- TIO.readFile "input/DayFive.txt"
    putStrLn $ "Day Five Part One: " ++ show (DayFive.resultOfRearrangement dayFiveInput)
    putStrLn $ "Day Five Part Two: " ++ show (DayFive.resultOfRearrangement' dayFiveInput)
    daySixInput <- TIO.readFile "input/DaySix.txt"
    putStrLn $ "Day Six Part One: " ++ show (DaySix.findStartOfPacketMarker daySixInput)
    putStrLn $ "Day Six Part Two: " ++ show (DaySix.findStartOfMessageMarker daySixInput)