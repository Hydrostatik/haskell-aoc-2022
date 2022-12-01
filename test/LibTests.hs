module Main (main) where

import Test.Hspec ( describe, it, shouldBe, Spec, hspec )

import DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought)

main :: IO ()
main = hspec testDayOne

testDayOne:: Spec
testDayOne = do
    describe "DayOne" $ do
        it "solves part one day one" $ do 
            highestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (24000 :: Int)
        it "solves part two day one" $ do
            topThreeHighestCaloriesBrought "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000" `shouldBe` (45000 :: Int)
