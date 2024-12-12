module Main where

import Advent.Utils (run)
import Data.Map qualified as M

type Input = Stones
type Stone = Int
type Stones = M.Map Stone Int

testInput :: Input
testInput =
    prepare
        [ "125 17"
        ]

-- Count number of digits in Int
numDigits :: Int -> Int
numDigits = length . show

-- Split into 2 stones e.g. 2001 = Map.fromlist [(20, 1) (1, 1)]
splitStone :: Stone -> Stones
splitStone stone = M.fromListWith (+) $ map ((,1) . read) [take m str, drop m str]
  where
    str = show stone
    m = length str `div` 2

step :: Stone -> Stones
step stone
    | stone == 0 = M.singleton 1 1
    | (even . numDigits) stone = splitStone stone
    | otherwise = M.singleton (2024 * stone) 1

blink :: Stones -> Stones
blink stones = M.unionsWith (+) $ do
    (stone, count) <- M.assocs stones
    pure (fmap (* count) (step stone))

solve :: Int -> Stones -> Int
solve n = sum . M.elems . (!! n) . iterate blink

part1 :: Input -> Int
part1 = solve 25

part2 :: Input -> Int
part2 = solve 75

prepare :: [String] -> Input
prepare = M.fromListWith (+) . map ((,1) . read) . words . head
>>>>>>> 19d4184179d0d7d11478b7022a1e27386f0f7bf6

main :: IO ()
main = run part1 part2 prepare
