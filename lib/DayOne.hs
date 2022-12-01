module DayOne (highestCaloriesBrought, topThreeHighestCaloriesBrought) where

import qualified Data.Text as T
import qualified Text.Read as TR

{-
Day 1: Calorie Counting
The Elves take turns writing down the number of Calories contained by the
various meals, snacks, rations, etc. that they've brought with them,
one item per line. Each Elf separates their own inventory from the
previous Elf's inventory (if any) by a blank line.
-}

type ElfLedger = T.Text
type Calories = Int

carriedByElves :: ElfLedger -> [Calories]
carriedByElves x = sum . fmap (unwrap . convertToInt) . T.lines <$> splitByElf x
    where
        splitByElf = T.splitOn "\n\n"
        convertToInt x = TR.readMaybe (T.unpack x) :: Maybe Int
        unwrap (Just x) = x
        unwrap Nothing = 0

-- Find the Elf carrying the most Calories and find out how many total Calories they are carrying.
highestCaloriesBrought :: ElfLedger -> Calories
highestCaloriesBrought x = maximum $ carriedByElves x

-- Find the top three Elves carrying the most Calories and find out how many total Calories they are carrying.
topThreeHighestCaloriesBrought :: ElfLedger -> Calories
topThreeHighestCaloriesBrought x = (\(x,y,z) -> x + y + z) $ foldl (flip retainTopThree) (0, 0, 0) (carriedByElves x)
    where
        retainTopThree x (y, z, w)
            |  x > y = (x, y , z)
            |  x > z = (y, x, z)
            |  x > w = (y, z, x)
            | otherwise = (y, z, w)