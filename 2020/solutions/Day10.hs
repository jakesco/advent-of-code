module Main where

import Advent.Graph (Graph)
import Advent.Graph qualified as Graph
import Advent.Utils (run)

import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map

type Input = [Int]

testInput :: Input
testInput =
    map
        read
        [ "16"
        , "10"
        , "15"
        , "5"
        , "1"
        , "11"
        , "7"
        , "19"
        , "6"
        , "12"
        , "4"
        ]

joltDiffs :: (Int, Int) -> Input -> (Int, Int)
joltDiffs _ [] = undefined -- unreachable
joltDiffs (acc1, acc3) [_] = (acc1, acc3 + 1)
joltDiffs (acc1, acc3) adapters@(x : y : _) =
    case y - x of
        1 -> joltDiffs (acc1 + 1, acc3) (tail adapters)
        3 -> joltDiffs (acc1, acc3 + 1) (tail adapters)
        _ -> error ("Not a jolt step of 1 or 3: " <> show (x, y))

part1 :: Input -> Int
part1 = uncurry (*) . joltDiffs (0, 0)

-- Part 2 ---------------------------------------------------------------

buildGraph :: Input -> Graph Int
buildGraph input =
    let
        graph = foldr Graph.addVertex Graph.empty input
     in
        addEdges graph input
  where
    addEdges :: Graph Int -> [Int] -> Graph Int
    addEdges graph [] = graph
    addEdges graph (x : xs) =
        let
            within3Jolts = filter (\y -> y - x <= 3 && y - x > 0) input
            newGraph = foldr (Graph.addDirectedEdge x) graph within3Jolts
         in
            addEdges newGraph xs

{- | Runs through each vertex, paths to each vertex
  are the sum of paths to its predecessors. Once we
  get to the final vertex we should have a count of
  all paths.
-}
countPaths :: Graph Int -> Int
countPaths g = snd . Map.findMax $ count Map.empty $ Graph.vertices g
  where
    count :: Map Int Int -> [Int] -> Map Int Int
    count paths [] = paths
    count paths (0 : vs) = count (Map.insert 0 1 paths) vs
    count paths (v : vs) =
        let
            predecessors = Graph.inNeighbors v g
            predPaths = Map.filterWithKey (\k _ -> k `elem` predecessors) paths
            sumOfPredPaths = Map.foldl' (+) 0 predPaths
            newPaths = Map.insert v sumOfPredPaths paths
         in
            count newPaths vs

part2 :: Input -> Int
part2 = countPaths . buildGraph

--------------------------------------------------------------------------

prepare :: String -> Input
prepare = (0 :) . sort . map read . lines

main :: IO ()
main = run part1 part2 prepare
