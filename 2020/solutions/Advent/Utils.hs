module Advent.Utils (run) where

import Control.Arrow ((&&&))
import System.Environment (getArgs)

run :: (Show b, Show c) => (a -> b) -> (a -> c) -> (String -> a) -> IO ()
run part1 part2 prepare = do
    args <- getArgs
    let inputFile = case args of
            (file : _) -> file
            [] -> "input.txt"
    readFile inputFile >>= print . (part1 &&& part2) . prepare
