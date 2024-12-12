module Main where

import Advent.Utils (run)

import Data.List (sort)
import Data.Maybe (fromMaybe)

type Seat = String
type Input = [String]

testInput :: Input
testInput =
    [ "FBFBBFFRLR"
    , "BFFFBBFRRR"
    , "FFFBBBFRRR"
    , "BBFFBBFRLL"
    ]

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

seatPos :: Seat -> (Int, Int)
seatPos seat = (findRow rowPart, findCol colPart)
  where
    (rowPart, colPart) = splitAt 7 seat
    findRow = binSearch (0, 127)
    findCol = binSearch (0, 8)

binSearch :: (Int, Int) -> String -> Int
binSearch (bottom, _) [] = bottom
binSearch bounds@(bottom, top) (curr : rest) =
    let
        step = (top - bottom + 1) `div` 2
        newBounds =
            if curr `elem` ['B', 'R']
                then (bottom + step, top)
                else (bottom, top - step)
     in
        binSearch newBounds rest

seatIds :: [Seat] -> [Int]
seatIds = map (seatId . seatPos)

searchForHole :: [Int] -> Maybe Int
searchForHole (x : y : rest)
    | y > x + 1 = Just (x + 1)
    | otherwise = searchForHole (y : rest)
searchForHole _ = Nothing

part1 :: Input -> Int
part1 = maximum . seatIds

part2 :: Input -> Int
part2 = fromMaybe (error "No seat found.") . searchForHole . sort . seatIds

prepare :: String -> Input
prepare = lines

main :: IO ()
main = run part1 part2 prepare
