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
type Move = Char

scoreByShape :: Move -> Score
scoreByShape 'X' = 1
scoreByShape 'Y' = 2
scoreByShape 'Z' = 3
scoreByShape  _  = 0

scoreByResult :: (Move, Move) -> Score
scoreByResult (opponentChoice, ourChoice)
    | opponentChoice == 'A' && ourChoice == 'Y' || opponentChoice == 'B' && ourChoice == 'Z' || opponentChoice == 'C' && ourChoice == 'X' = 6
    | opponentChoice == 'A' && ourChoice == 'X' || opponentChoice == 'B' && ourChoice == 'Y' || opponentChoice == 'C' && ourChoice == 'Z' = 3
    | otherwise = 0

mutateForOutcome :: (Move, Move) -> (Move, Move)
mutateForOutcome (opponentChoice, 'X') = (opponentChoice, losingMove opponentChoice)
mutateForOutcome (opponentChoice, 'Y') = (opponentChoice, drawingMove opponentChoice)
mutateForOutcome (opponentChoice, 'Z') = (opponentChoice, winningMove opponentChoice)

winningMove :: Move -> Move
winningMove 'A' = 'Y'
winningMove 'B' = 'Z'
winningMove 'C' = 'X'

drawingMove :: Move -> Move
drawingMove 'A'= 'X'
drawingMove 'B'= 'Y'
drawingMove 'C'= 'Z'

losingMove :: Move -> Move
losingMove 'A' = 'Z'
losingMove 'B' = 'X'
losingMove 'C' = 'Y'

-- What would your total score be if everything goes exactly according to your strategy guide?
personalHighScore :: ScoreSheet -> Score
personalHighScore x = sum ((\(x, y) -> scoreByShape y + scoreByResult (x, y)) . (\x -> (T.head $ head x, T.head $ last x)) . T.words <$> T.lines x)

-- If X actually instructed you to lose, Y instructed you to draw and Z instructed you to win, then what would your total score be?
personalHighScore' :: ScoreSheet -> Score
personalHighScore' x = sum ((\(x, y) -> scoreByShape y + scoreByResult (x, y)) . (\x -> mutateForOutcome (T.head $ head x, T.head $ last x)) . T.words <$> T.lines x)