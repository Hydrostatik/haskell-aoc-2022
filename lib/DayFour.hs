module DayFour (totalAssignmentOverlaps, totalAssignmentOverlaps') where

import qualified Data.Text as T
import qualified Data.Bifunctor as B

{-
Day 4: Camp Cleanup
Several Elves have been assigned the job of cleaning up
sections of the camp. every section has a unique ID number,
and each Elf is assigned a range of section IDs

However, several Elves have noticed that their assignments overlap.
To try to quickly find overlaps and reduce duplicated effort, the 
Elves pair up and make a big list of the section assignments for each pair
-}

type AssignmentLedger = T.Text
type Overlaps = Int

cleanSplit :: Eq a => a -> [a] -> ([a], [a])
cleanSplit char x = B.second tail $ span (/= char) x

readRange :: (String, String) -> (Int, Int)
readRange (x, y) = (lowerBound, upperBound)
    where
        lowerBound = read x :: Int
        upperBound = read y :: Int

parseRange :: [Char] -> (Int, Int)
parseRange = readRange . cleanSplit '-'

isSuperSet :: (Int, Int) -> (Int , Int) -> Bool
isSuperSet (x, y) (x1, y1)
    | x >= x1 && y <= y1 = True
    | x <= x1 && y >= y1 = True
    | otherwise = False

isOverlap :: (Int, Int) -> (Int, Int) -> Bool
isOverlap (x, y) (x1, y1)
    | x >= x1 && x <= y1 = True
    | x1 >= x && x1 <= y = True
    | otherwise = False

-- In how many assignment pairs does one range fully contain the other?
totalAssignmentOverlaps :: AssignmentLedger -> Overlaps
totalAssignmentOverlaps x = length $ filter id $ uncurry isSuperSet . B.bimap parseRange parseRange . cleanSplit ',' . T.unpack <$> T.lines x

-- In how many assignment pairs does one range contain some part of the other?
totalAssignmentOverlaps' :: AssignmentLedger -> Overlaps
totalAssignmentOverlaps' x = length $ filter id $ uncurry isOverlap . B.bimap parseRange parseRange . cleanSplit ',' . T.unpack <$> T.lines x
        