module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (combinations, run)
import Control.Arrow ((&&&))
import Data.Char (isAlpha, isDigit)
import Data.List (nub)

type Input = (String, Grid Char)

data Slope = Slope Int Int
    deriving (Eq, Show)

testInput :: Input
testInput =
    prepare
        [ "............"
        , "........0..."
        , ".....0......"
        , ".......0...."
        , "....0......."
        , "......A....."
        , "............"
        , "............"
        , "........A..."
        , ".........A.."
        , "............"
        , "............"
        ]

-- Keep then rise/run separate to play nice with
-- integer grid
findSlope :: Position -> Position -> Slope
findSlope (x1, y1) (x2, y2) =
    -- Always order left to right
    if x1 <= x2
        then Slope (y2 - y1) (x2 - x1)
        else Slope (y1 - y2) (x1 - x2)

findAntinodes :: Position -> Position -> [Position]
findAntinodes a@(x1, y1) b@(x2, y2) =
    let
        (Slope rise' run') = findSlope a b
     in
        -- Apply slope to furthest right
        -- and -slope to furthes left
        if x1 <= x2
            then
                [(x1 - run', y1 - rise'), (x2 + run', y2 + rise')]
            else
                [(x1 + run', y1 + rise'), (x2 - run', y2 - rise')]

-- Same as findAntinodes but casts out enough in either direction
-- to well past grid bounds (50 is max for my input)
findAntinodes2 :: Position -> Position -> [Position]
findAntinodes2 a@(x1, y1) b =
    let
        (Slope rise' run') = findSlope a b
     in
        -- -50 to 50 should be more than enough
        [(x1 + run' * n, y1 + rise' * n) | n <- [-50 .. 50]]

calcAntinodes :: (Position -> Position -> [Position]) -> Input -> Int
calcAntinodes findFunction (freqs, grid) =
    let
        towers = concatMap (combinations 2 . flip Grid.findAll grid . (==)) freqs
        antinodes = concatMap (\nodes -> findFunction (head nodes) (head $ tail nodes)) towers
     in
        length $ filter (Grid.inBounds grid) (nub antinodes)

part1 :: Input -> Int
part1 = calcAntinodes findAntinodes

part2 :: Input -> Int
part2 = calcAntinodes findAntinodes2

prepare :: [String] -> Input
prepare =
    (nub . filter (\x -> isDigit x || isAlpha x) . unlines)
        &&& Grid.fromList

main :: IO ()
main = run part1 part2 prepare
