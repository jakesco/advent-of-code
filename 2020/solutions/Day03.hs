module Main where

import Advent.Utils (run)

type Input = [String]

testInput :: Input
testInput =
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]

prepareGrid :: Int -> Int -> Input -> [(Int, String)]
prepareGrid r d g =
    zip (rows r) (downSlope d g)
  where
    downSlope :: Int -> Input -> Input
    downSlope d [] = []
    downSlope d (l : ls) = l : downSlope d (drop (d - 1) ls)

    rows :: Int -> [Int]
    rows r = [0 + r * n | n <- [0 ..]]

checkSlope :: Int -> Int -> Input -> Int
checkSlope r d g =
    foldl (\acc (row, line) -> if cycle line !! row == '#' then acc + 1 else acc) 0 $ prepareGrid r d g

part1 :: Input -> Int
part1 = checkSlope 3 1

part2 :: Input -> Int
part2 g =
    let
        slopes =
            [ (1, 1)
            , (3, 1)
            , (5, 1)
            , (7, 1)
            , (1, 2)
            ]
     in
        foldl (\acc (r, d) -> acc * checkSlope r d g) 1 slopes

prepare :: String -> Input
prepare = lines

main :: IO ()
main = run part1 part2 prepare
