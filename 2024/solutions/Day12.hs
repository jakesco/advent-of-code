module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (run)
import Data.List (nub)

type Input = Grid Char

type Id = Char
type Area = Int
type Perimeter = Int

data Region = Region Id Area Perimeter
    deriving (Eq, Show)

testInput :: Input
testInput =
    prepare
        [ "OOOOO"
        , "OXOXO"
        , "OOOOO"
        , "OXOXO"
        , "OOOOO"
        ]

fencePrice :: Region -> Int
fencePrice (Region _ x y) = x * y

uniqueRegions :: Input -> [Char]
uniqueRegions = nub . Grid.getAllVals

findRegion :: Input -> Char -> Region
findRegion grid c =
    let
        plots = Grid.findAll (== c) grid
     in
        Region c (length plots) (sum $ map (countPerimeterPlot grid c) plots)

countPerimeterPlot :: Input -> Char -> Position -> Int
countPerimeterPlot g target pos =
    4 - length (filter (== target) neighbors)
  where
    neighbors = Grid.cardinalNeighborVals g pos

part1 :: Input -> Int
part1 grid = sum $ fencePrice . findRegion grid <$> uniqueRegions grid

part2 :: Input -> ()
part2 = const ()

prepare :: [String] -> Input
prepare = Grid.fromList

main :: IO ()
main = run part1 part2 prepare
