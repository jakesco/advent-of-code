module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (run)

type Input = Grid Char
type RuleBuilder = Grid Char -> (Position -> Char -> Char)

testInput :: Input
testInput =
    Grid.fromList
        [ "L.LL.LL.LL"
        , "LLLLLLL.LL"
        , "L.L.L..L.."
        , "LLLL.LL.LL"
        , "L.LL.LL.LL"
        , "L.LLLLL.LL"
        , "..L.L....."
        , "LLLLLLLLLL"
        , "L.LLLLLL.L"
        , "L.LLLLL.LL"
        ]

-- Rules
--  1. If a seat is L and there are no occupied adjacent seats it becomes #
--  2. If a seat is # and four or more seats adjacent ar # seat becoms L
--  3. Otherwise seat stays the same.
rules1 :: RuleBuilder
rules1 g = applyRule
  where
    applyRule :: Position -> Char -> Char
    applyRule pos c
        | c == '.' = '.'
        | c == 'L' && '#' `notElem` Grid.neighborVals g pos = '#'
        | c == '#' && fourOrMoreAdjOccupied pos = 'L'
        | otherwise = c

    fourOrMoreAdjOccupied :: Position -> Bool
    fourOrMoreAdjOccupied pos =
        (>= 4) . length $ filter (== '#') (Grid.neighborVals g pos)

rules2 :: RuleBuilder
rules2 g = undefined

-- Run simulation until steady state
runSim :: RuleBuilder -> Grid Char -> Grid Char
runSim rules g =
    let
        newGrid = Grid.mapWithPosition (rules g) g
     in
        if g == newGrid then g else runSim rules newGrid

countOccupied :: Grid Char -> Int
countOccupied g = foldl (\acc x -> if x == '#' then acc + 1 else acc) 0 $ Grid.getAllVals g

part1 :: Input -> Int
part1 = countOccupied . runSim rules1

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = Grid.fromList . lines

main :: IO ()
main = run part1 part2 prepare
