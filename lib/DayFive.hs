module DayFive (resultOfRearrangement, resultOfRearrangement') where

import qualified Data.Text as T
import qualified Data.Bifunctor as B
import qualified Data.Map as M
import qualified Data.Char as C
import qualified Data.List as L

{-
Day 5: Supply Stacks

The expedition can depart as soon as the final supplies have been
unloaded from the ships. Supplies are stored in stack of marked crates,
but because the needed supplies are buried under many other crates, the
crates need to be rearranged.

The giant cargo crane can rearrange the crates in a series of carefully-planned
steps. After the crates are rearranged, the desired crates will be at the top
of each stack.

The Elves don't want to interrupt the process, but they forgot to ask 
which crate will end up where, and they want to be ready to unload them
as soon as possible so they can embark.
-}

type ProcedureManual = T.Text
type CratePriorityMessage = T.Text

cleanSplit :: Eq a => a -> [a] -> ([a], [a])
cleanSplit char x = B.second tail $ span (/= char) x

parseInitialStackCondition :: [T.Text] -> [T.Text]
parseInitialStackCondition xs = filter (/= "") $ fmap (T.pack . filter C.isAlpha) $ L.transpose $ T.unpack <$> xs

parseInstruction :: T.Text -> [Int]
parseInstruction x = read . T.unpack <$> T.words (T.filter (\x -> C.isDigit x || C.isSpace x) x)

executeInstruction :: [T.Text] -> [Int] -> [T.Text]
executeInstruction xs i = (\(x, y) -> 
    if y == from 
        then T.drop (T.length cratesTaken) x 
    else if y == to 
        then cratesTaken <> x
    else x) <$> zip xs [0..]
    where
        cratesTaken = T.reverse $ T.take crates (xs !! from) 
        crates = head i
        from = 1 `subtract` (i !! 1)
        to = 1 `subtract` (i !! 2)

executeInstruction' :: [T.Text] -> [Int] -> [T.Text]
executeInstruction' xs i = (\(x, y) -> 
    if y == from 
        then T.drop (T.length cratesTaken) x 
    else if y == to 
        then cratesTaken <> x
    else x) <$> zip xs [0..]
    where
        cratesTaken = T.take crates (xs !! from) 
        crates = head i
        from = 1 `subtract` (i !! 1)
        to = 1 `subtract` (i !! 2)

-- determine which crate will end up on top of each stack
resultOfRearrangement :: ProcedureManual -> CratePriorityMessage
resultOfRearrangement x = T.pack $ fmap T.head $ (\(x, y) -> L.foldl' executeInstruction (parseInitialStackCondition x) (parseInstruction <$> y)) $ cleanSplit "" $ T.lines x

-- determine which crate will end up on top if the crates remain in the same order while being moved
resultOfRearrangement' :: ProcedureManual -> CratePriorityMessage
resultOfRearrangement' x = T.pack $ fmap T.head $ (\(x, y) -> L.foldl' executeInstruction' (parseInitialStackCondition x) (parseInstruction <$> y)) $ cleanSplit "" $ T.lines x