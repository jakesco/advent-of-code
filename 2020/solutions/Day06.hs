module Main where

import Advent.Utils (run)
import Data.Set (Set)
import Data.Set qualified as S

type Input = [String]

testInput :: Input
testInput =
    [ "abc"
    , ""
    , "a"
    , "b"
    , "c"
    , ""
    , "ab"
    , "ac"
    , ""
    , "a"
    , "a"
    , "a"
    , "a"
    , ""
    , "b"
    ]

collectAnswers :: (Ord a) => [[a]] -> [[Set a]]
collectAnswers [] = []
collectAnswers s =
    let
        (a, b) = break null s
     in
        if null b
            then
                [map S.fromList a]
            else
                map S.fromList a : collectAnswers (tail b)

intersections :: (Ord a) => [Set a] -> Set a
intersections (x : xs) = foldl S.intersection x xs

part1 :: Input -> Int
part1 = sum . map (length . S.unions) . collectAnswers

part2 :: Input -> Int
part2 = sum . map (length . intersections) . collectAnswers

prepare :: String -> Input
prepare = lines

main :: IO ()
main = run part1 part2 prepare
