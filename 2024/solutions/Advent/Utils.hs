module Advent.Utils (run, combinations) where

import Control.Arrow ((&&&))
import Data.List (subsequences)
import System.Environment (getArgs)

-- | Run part1 and part2 on prepared input, then print result.
run :: (Show b, Show c) => (a -> b) -> (a -> c) -> ([String] -> a) -> IO ()
run part1 part2 prepare = do
    args <- getArgs
    let inputFile = case args of
            (file : _) -> file
            [] -> "input.txt"
    readFile inputFile >>= print . (part1 &&& part2) . prepare . lines

-- | Generate combinations of elements from list
combinations :: Int -> [a] -> [[a]]
combinations n ls = filter ((== n) . length) (subsequences ls)
