module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (run)
import Data.Char (digitToInt)

type Input = Grid Int

testInput :: Input
testInput =
  prepare
    [ "89010123"
    , "78121874"
    , "87430965"
    , "96549874"
    , "45678903"
    , "32019012"
    , "01329801"
    , "10456732"
    ]

{- | Take a starting positon and follows the trial to all
  9 height endpoints. Returns the number of 9 endpoits.
-}
scoreTrail :: Input -> Position -> Int
scoreTrail g start = length $ filter id $ search (getValidNieghbors start) [] start <$> Grid.findAll (== 9) g
 where
  getValue :: Position -> Int
  getValue = Grid.getWithDefault g 100

  oneStep :: Position -> Position -> Bool
  oneStep p1 p2 = getValue p2 == succ (getValue p1)

  getValidNieghbors :: Position -> [Position]
  getValidNieghbors p = filter (oneStep p) (Grid.cardinalNeighbors g p)

  search :: [Position] -> [Position] -> Position -> Position -> Bool
  search [] _ curr target = curr == target
  search (v : vs) visited curr target
    | curr == target = True
    | otherwise =
        let
          newVisited = curr : visited
          newToVisit = filter (`notElem` newVisited) (getValidNieghbors curr) <> vs
         in
          search newToVisit newVisited v target

part1 :: Input -> Int
part1 trailMap = sum $ scoreTrail trailMap <$> Grid.findAll (== 0) trailMap

part2 :: Input -> ()
part2 = const ()

prepare :: [String] -> Input
prepare = fmap digitToInt . Grid.fromList

main :: IO ()
main = run part1 part2 prepare
