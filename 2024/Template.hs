module Main where

import Advent.Utils (run)

type Input = [String]

testInput :: Input
testInput = prepare []

part1 :: Input -> ()
part1 = const ()

part2 :: Input -> ()
part2 = const ()

prepare :: [String] -> Input
prepare = id

main :: IO ()
main = run part1 part2 prepare
