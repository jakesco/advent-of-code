module Main where

import Advent.Utils (run)

import Data.Maybe (fromJust)

import Control.Monad (msum)

type Input = [Int]

testInput :: [String]
testInput =
    [ "35"
    , "20"
    , "15"
    , "25"
    , "47"
    , "40"
    , "62"
    , "55"
    , "65"
    , "95"
    , "102"
    , "117"
    , "150"
    , "182"
    , "127"
    , "219"
    , "299"
    , "277"
    , "309"
    , "576"
    ]

slice :: Int -> Int -> [Int] -> [Int]
slice start end = take (end - start) . drop start

buildSums :: [Int] -> [Int]
buildSums ls = concatMap (\a -> map (+ a) ls) ls

findContiguous :: Int -> Int -> Int -> [Int] -> Maybe (Int, Int)
findContiguous target startIdx currIdx ls =
    path $ sum $ slice startIdx currIdx ls
  where
    path n
        | n > target = Nothing
        | n == target = Just (startIdx, currIdx)
        | otherwise = findContiguous target startIdx (currIdx + 1) ls

search :: Int -> Int -> Input -> Maybe Int
search preamble current ls
    | current >= length ls = Nothing
    | otherwise =
        let
            i = ls !! current
            sums = buildSums (slice (current - preamble) current ls)
         in
            if i `elem` sums
                then
                    search preamble (current + 1) ls
                else
                    Just i

part1 :: Input -> Int
part1 = fromJust . search 25 25

part2 :: Input -> Int
part2 ls =
    let
        target = part1 ls
     in
        weakness $ msum $ map (\x -> findContiguous target x x ls) [0 .. length ls]
  where
    weakness :: Maybe (Int, Int) -> Int
    weakness (Just (x, y)) =
        let
            s = slice x y ls
         in
            maximum s + minimum s
    weakness _ = 0

prepare :: String -> Input
prepare = map read . lines

main :: IO ()
main = run part1 part2 prepare
