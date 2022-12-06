module DaySix (findStartOfPacketMarker, findStartOfMessageMarker) where

import qualified Data.Text as T
import qualified Data.Set as S

{-
Day 6: Tuning Trouble
As you move through the dense undergrowth, one of the Elves
gives you a handheld device with a communication system.
However, you are given a malfunctioning device.

To communicate with the Elves, the device needs to lock on
to their signal. The signal is a series of seemingly-random characters
that the device receives one at a time.

To fix the communication system, you need to add a subroutine to the
device that detects a start-of-packet marker in the datastream. The
start of packet is indicated by a sequence of four characters that are all different.
-}

type Packet = T.Text
type Index = Int

group :: Int -> [a] -> [[a]]
group x ys = take x ys : group x (drop 1 ys)

isPacketUnique :: [Char] -> Bool
isPacketUnique = uniqueness S.empty
    where
        uniqueness set (x:xs) = not (x `S.member` set) && uniqueness (x `S.insert` set) xs
        uniqueness set [] = True

-- How many characters need to be processed before the first start-of-packet marker is detected?
findStartOfPacketMarker :: Packet -> Index
findStartOfPacketMarker = fst . last . head . dropWhile (not . isPacketUnique . fmap snd) . group 4 . zip [1..] . T.unpack

-- How many characters need to be processed before the first start-of-message marker (14 consecutive unique characters) is detected?
findStartOfMessageMarker :: Packet -> Index
findStartOfMessageMarker = fst . last . head . dropWhile (not . isPacketUnique . fmap snd) . group 14 . zip [1..] . T.unpack