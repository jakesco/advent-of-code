module Main where

import Advent.Utils (run)

type Input = [[Int]]

testInput :: Input
testInput =
    prepare
        [ "7 6 4 2 1"
        , "1 2 7 8 9"
        , "9 7 6 2 1"
        , "1 3 2 4 5"
        , "8 6 4 4 1"
        , "1 3 6 7 9"
        ]

safeInc :: [Int] -> Bool
safeInc report@(x : y : _) =
    (y > x && y - x <= 3) && safeInc (tail report)
safeInc _ = True

safeDec :: [Int] -> Bool
safeDec report@(x : y : _) =
    (y < x && x - y <= 3) && safeDec (tail report)
safeDec _ = True

tryRemoval :: ([Int] -> Bool) -> [Int] -> Bool
tryRemoval f report =
    f report || any f [take i report ++ drop (i + 1) report | i <- [0 .. length report - 1]]

part1 :: Input -> Int
part1 = length . filter (\xs -> safeInc xs || safeDec xs)

part2 :: Input -> Int
part2 = length . filter (\xs -> tryRemoval safeInc xs || tryRemoval safeDec xs)

prepare :: [String] -> Input
prepare = map (map read . words)

main :: IO ()
main = run part1 part2 prepare
