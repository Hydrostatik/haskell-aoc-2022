module Main (main) where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )
import qualified Data.Text as T

import DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)
import DayTwo (personalHighScore, personalHighScore')
import DayThree (totalRucksackOrderPriority, totalBadgePriority)
import DayFour (totalAssignmentOverlaps, totalAssignmentOverlaps')
import DayFive (resultOfRearrangement, resultOfRearrangement')

main :: IO ()
main =  mconcat $ fmap hspec [testDayOne, testDayTwo, testDayThree, testDayFour, testDayFive]

testDayOne :: Spec
testDayOne = do
    describe "DayOne" $ do
        it "solves part one day one" $ do 
            highestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (24000 :: Int)
        it "solves part two day one" $ do
            topThreeHighestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (45000 :: Int)

testDayTwo :: Spec
testDayTwo = do
    describe "DayTwo" $ do
        it "solves part one day two" $ do
            personalHighScore "A Y\nB X\nC Z" `shouldBe` (15 :: Int)
        it "solves part two day two" $ do
            personalHighScore' "A Y\nB X\nC Z" `shouldBe` (12 :: Int)

testDayThree :: Spec
testDayThree = do 
    describe "DayThree" $ do
        it "solves part one day three" $ do
            totalRucksackOrderPriority "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" `shouldBe` (157 :: Int)
        it "solves part two day three" $ do
            totalBadgePriority "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw" `shouldBe` (70 :: Int)

testDayFour :: Spec
testDayFour = do
    describe "DayFour" $ do
        it "solves part one day four" $ do
            totalAssignmentOverlaps "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" `shouldBe` (2 :: Int)
        it "solves part two day four" $ do
            totalAssignmentOverlaps' "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8" `shouldBe` (4 :: Int)

testDayFive :: Spec
testDayFive = do
    describe "DayFive" $ do
        it "solves part one day five" $ do
            resultOfRearrangement "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2" `shouldBe` ("CMZ" :: T.Text)
        it "solves part two day five" $ do
            resultOfRearrangement' "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2" `shouldBe` ("MCD" :: T.Text)
