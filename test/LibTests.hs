module Main (main) where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )

import DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)
import DayTwo (personalHighScore, personalHighScore')

main :: IO ()
main =  mconcat $ fmap hspec [testDayOne, testDayTwo]

testDayOne:: Spec
testDayOne = do
    describe "DayOne" $ do
        it "solves part one day one" $ do 
            highestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (24000 :: Int)
        it "solves part two day one" $ do
            topThreeHighestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (45000 :: Int)

testDayTwo:: Spec
testDayTwo = do
    describe "DayTwo" $ do
        it "solves part one day two" $ do
            personalHighScore "A Y\nB X\nC Z" `shouldBe` (15 :: Int)
        it "solves part two day two" $ do
            personalHighScore' "A Y\nB X\nC Z" `shouldBe` (12 :: Int)