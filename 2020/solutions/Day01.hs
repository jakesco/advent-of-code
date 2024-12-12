module Main where

import Advent.Utils (run)
import Data.List (tails)

type Input = [Int]

testInput :: Input
testInput =
    [ 1721
    , 979
    , 366
    , 299
    , 675
    , 1456
    ]

combinations :: [a] -> [(a, a)]
combinations xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

combinations3 :: [a] -> [[a]]
combinations3 xs = [[x, y, z] | (x : rest) <- tails xs, (y : rest') <- tails rest, z <- rest']

part1 :: Input -> Int
part1 =
    uncurry (*) . head . filter (\(a, b) -> a + b == 2020) . combinations

part2 :: Input -> Int
part2 =
    product . head . filter (\xs -> sum xs == 2020) . combinations3

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = run part1 part2 prepare
