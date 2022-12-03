module DayThree (totalRucksackOrderPriority, totalBadgePriority) where

import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.List as L

{-
Day Three: Rucksack Reorganization
One Elf has the important job of loading all of the ruckacks with
supplies for the jungle journey. Unfortunately, that Elf didn't quite
follow the packing instructions, and so a few items now need to be rearranged.

Each rucksack has two large components. All items of a given type are meant to go into
exactly one of those compartments. The Elf that did the packing failed to follow this rule for exactly
one item type per rucksack.

You need to identify the error. (Note: a and A refer to different items)

To help prioritize item rearrangement:
- Lowercase item types a through Z have priorities 1 through 26.
- Uppercase item types A through Z have priorities 27 through 52.
-}

type RucksackLedger = T.Text
type TotalItemPriority = Int

groupInThree :: [T.Text] -> [[T.Text]]
groupInThree [] = []
groupInThree (x:y:z:rest) = [x, y, z] : groupInThree rest

-- What is the sum of the priorities of those item types?
totalRucksackOrderPriority :: RucksackLedger -> TotalItemPriority
totalRucksackOrderPriority x =  sum $ sum . fmap ((\x -> if x < 97 then x - 38 else x - 96) . C.ord) . (\(x,y) -> (\x -> [head x]) $ T.unpack x `L.intersect` T.unpack y) . (\x -> T.splitAt (T.length x `div` 2) x) <$> T.lines x

-- What is the sum of the badge item type. (The badge item type is the item identifier common among three rucksacks or "group of elves")
totalBadgePriority :: RucksackLedger -> TotalItemPriority
totalBadgePriority x = sum $ ((\x -> if x < 97 then x - 38 else x - 96) . C.ord) . (\x -> head $ L.foldl' L.intersect (head x) (tail x)) . fmap T.unpack <$> groupInThree (T.lines x)