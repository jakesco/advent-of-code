module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (run)

type Input = Grid Char

testInput :: Input
testInput =
    prepare
        [ "MMMSXXMASM"
        , "MSAMXMSMSA"
        , "AMXSXMAAMM"
        , "MSAMASMSMX"
        , "XMASAMXAMM"
        , "XXAMMXXAMA"
        , "SMSMSASXSS"
        , "SAXAMASAAA"
        , "MAMMMXMMMM"
        , "MXMXAXMASX"
        ]

directions :: [[(Char, Position)]]
directions =
    map
        (zip "XMAS")
        [ [(0, -x) | x <- [0 .. 3]] -- up
        , [(0, x) | x <- [0 .. 3]] -- down
        , [(-x, 0) | x <- [0 .. 3]] -- left
        , [(x, 0) | x <- [0 .. 3]] -- right
        , [(-x, -x) | x <- [0 .. 3]] -- up left
        , [(x, -x) | x <- [0 .. 3]] -- up right
        , [(x, x) | x <- [0 .. 3]] -- down right
        , [(-x, x) | x <- [0 .. 3]] -- down left
        ]

{- | Searches in every direction, counting the number
  of valid XMAS.
-}
searchXmas :: Input -> Position -> Int
searchXmas g pos = case Grid.get g pos of
    (Just 'X') -> length $ filter (searchDirection g pos) directions
    _ -> 0

searchDirection :: Input -> Position -> [(Char, Position)] -> Bool
searchDirection _ _ [] = True
searchDirection g pos@(x, y) ((expected, (dx, dy)) : rest) =
    let
        next = (x + dx, y + dy)
        actual = Grid.getWithDefault g '.' next
     in
        (actual == expected)
            && searchDirection g pos rest

searchXmas2 :: Input -> Position -> Bool
searchXmas2 grid pos =
    let
        searchA = searchDirectionA grid pos
        searchB = searchDirectionB grid pos
     in
        (searchA 'M' 'S' || searchA 'S' 'M')
            && (searchB 'M' 'S' || searchB 'S' 'M')

searchDirectionA :: Input -> Position -> Char -> Char -> Bool
searchDirectionA g (x, y) a b =
    Grid.getWithDefault g '.' (x + 1, y - 1) == a
        && Grid.getWithDefault g '.' (x - 1, y + 1) == b

searchDirectionB :: Input -> Position -> Char -> Char -> Bool
searchDirectionB g (x, y) a b =
    Grid.getWithDefault g '.' (x - 1, y - 1) == a
        && Grid.getWithDefault g '.' (x + 1, y + 1) == b

checkChar :: Input -> Char -> Position -> Bool
checkChar g c pos =
    Grid.getWithDefault g '.' pos == c

part1 :: Input -> Int
part1 g = sum $ searchXmas g <$> Grid.findAll (== 'X') g

part2 :: Input -> Int
part2 g = length $ filter (searchXmas2 g) $ Grid.findAll (== 'A') g

prepare :: [String] -> Input
prepare = Grid.fromList

main :: IO ()
main = run part1 part2 prepare
