module Main where

import Advent.Utils (run)
import Control.Arrow ((***))
import Data.List (sort)

type Input = ([Int], [Int])

testInput :: Input
testInput =
    prepare
        [ "3   4"
        , "4   3"
        , "2   5"
        , "1   3"
        , "3   9"
        , "3   3"
        ]

part1 :: Input -> Int
part1 = sum . fmap (abs . uncurry (-)) . uncurry zip . (sort *** sort)

part2 :: Input -> Int
part2 (as, bs) = sum [a * count a bs | a <- as]
  where
    count x = length . filter (== x)

prepare :: [String] -> Input
prepare =
    foldr
        ( \s (l1, l2) -> case read <$> words s of
            [a, b] -> (a : l1, b : l2)
            _ -> error ("Failed to parse input: " <> s)
        )
        ([], [])

main :: IO ()
main = run part1 part2 prepare
