module DayTwo (personalHighScore, personalHighScore') where

import qualified Data.Text as T

{-
Day 2: Rock Paper Scissors
Appreciative of your help yesterday, one Elf gives you an encrypted
strategy guide (your puzzle input) that they say will be sure to help you win.

The first column is what your opponent is going to play:
A for Rock,
B for Paper,
C for Scissors

The second column you reason is what you must play in response:
X for Rock,
Y for Paper,
Z for Scissors

The winner of the whole tournament is the player with the highest score.
Your total score is the sum of your scores for each round. The score for a single round is:
1. The score for the shape you selected (1 for Rock, 2 for Paper, 3 for Scissors)
2. The score for the outcome of the round (0 if you lost, 3 if the round was a draw, 6 if you won)
-}

type ScoreSheet = T.Text
type Score = Int

class (Enum a, Bounded a, Eq a) => Circular a where
     next :: a -> a
     next a = if a == maxBound then minBound else succ a

data Move = Rock | Paper | Scissors deriving (Show, Enum, Bounded, Eq)
data Outcome = Win | Lose | Draw

instance Circular Move

scoreByShape :: Move -> Score
scoreByShape Rock = 1
scoreByShape Paper = 2
scoreByShape Scissors = 3

decodeOpponentMove :: Char -> Move
decodeOpponentMove 'A' = Rock
decodeOpponentMove 'B' = Paper
decodeOpponentMove 'C' = Scissors

decodePlayerMove :: Char -> Move
decodePlayerMove 'X' = Rock
decodePlayerMove 'Y' = Paper
decodePlayerMove 'Z' = Scissors

decodeOutcome :: Char -> Outcome
decodeOutcome 'X' = Lose
decodeOutcome 'Y' = Draw
decodeOutcome 'Z' = Win

scoreByResult :: Move -> Move -> Score
scoreByResult x y
    | next x == y = 6
    | x == y = 3
    | otherwise = 0

moveForOutcome :: Move -> Outcome -> (Move, Move)
moveForOutcome x Lose = (x, next $ next x)
moveForOutcome x Draw = (x, x)
moveForOutcome x Win = (x, next x)

-- What would your total score be if everything goes exactly according to your strategy guide?
personalHighScore :: ScoreSheet -> Score
personalHighScore x = sum ((\(x, y) -> scoreByShape y + scoreByResult x y) . (\x -> (decodeOpponentMove $ T.head $ head x, decodePlayerMove $ T.head $ last x)) . T.words <$> T.lines x)

-- If X actually instructed you to lose, Y instructed you to draw and Z instructed you to win, then what would your total score be?
personalHighScore' :: ScoreSheet -> Score
personalHighScore' x = sum ((\(x, y) -> scoreByShape y + scoreByResult x y) . (\x -> moveForOutcome (decodeOpponentMove $ T.head $ head x) (decodeOutcome $ T.head $ last x)) . T.words <$> T.lines x)