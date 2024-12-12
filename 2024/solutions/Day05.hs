module Main where

import Advent.Utils (run)
import Control.Arrow ((***))
import Data.List (find, sortBy)
import Data.List.Split (splitOn)

type Input = (SortRules, Updates)

data Order = Order Int Int deriving (Eq, Show)
type SortRules = [Order]
type Updates = [[Int]]

testInput :: Input
testInput =
    prepare
        [ "47|53"
        , "97|13"
        , "97|61"
        , "97|47"
        , "75|29"
        , "61|13"
        , "75|53"
        , "29|13"
        , "97|29"
        , "53|29"
        , "61|53"
        , "97|53"
        , "61|29"
        , "47|13"
        , "75|47"
        , "97|75"
        , "47|61"
        , "75|61"
        , "47|29"
        , "75|13"
        , "53|13"
        , ""
        , "75,47,61,53,29"
        , "97,61,53,29,13"
        , "75,29,13"
        , "75,97,47,61,53"
        , "61,13,29"
        , "97,13,75,29,47"
        ]

getMiddle :: [a] -> a
getMiddle ls = ls !! (length ls `div` 2)

isSorted :: (Int -> Int -> Ordering) -> [Int] -> Bool
isSorted f ls = ls == sortBy f ls

{- | After messing with this for a while, I noticed
 all numbers which appear next to each other has an
 associated rule. There is no need to check for
 transitive rules, so this niave approach works.
-}
sortFunc :: [Order] -> (Int -> Int -> Ordering)
sortFunc orders a b =
    case find (\(Order x y) -> x == a && y == b) orders of
        (Just _) -> LT
        Nothing -> GT

part1 :: Input -> Int
part1 (orders, updates) =
    sum $ getMiddle <$> filter (isSorted $ sortFunc orders) updates

part2 :: Input -> Int
part2 (orders, updates) =
    sum $ getMiddle . sortBy f <$> filter (not . isSorted f) updates
  where
    f = sortFunc orders

prepare :: [String] -> Input
prepare = (map parseOrder *** (map parseUpdate . tail)) . break (== "")
  where
    parseOrder :: String -> Order
    parseOrder s = case read <$> splitOn "|" s of
        [a, b] -> Order a b
        _ -> error ("Failed to parse order rule: " <> s)

    parseUpdate :: String -> [Int]
    parseUpdate = map read . splitOn ","

main :: IO ()
main = run part1 part2 prepare
