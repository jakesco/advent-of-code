module Main where

import Advent.Utils (run)
import Control.Arrow ((&&&))
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe, isJust, isNothing)

type Input = [Maybe Int]

-- alias just to keep functions organized
type PackedBlock = [Maybe Int]

-- NOTE: Look into segment trees

testInput :: Input
testInput =
    prepare
        [ "2333133121414131402"
        ]

-- unpack input string
inflate :: String -> Input
inflate = go 0 0
  where
    go :: Int -> Int -> String -> Input
    go _ _ [] = []
    go idx fileId (d : ds) =
        let
            count = digitToInt d
         in
            if even idx
                then replicate count (Just fileId) <> go (idx + 1) (fileId + 1) ds
                else replicate count Nothing <> go (idx + 1) fileId ds

pack :: Input -> PackedBlock
pack block = go 0 block (reverse $ filter isJust block)
  where
    expectedLen :: Int
    expectedLen = uncurry (-) $ (length &&& (length . filter isNothing)) block

    go :: Int -> Input -> [Maybe Int] -> PackedBlock
    go len (x : xs) rev@(y : ys)
        | len >= expectedLen = []
        | otherwise =
            if isNothing x
                then y : go (succ len) xs ys
                else x : go (succ len) xs rev
    go _ _ _ = []

packWholeFiles :: Input -> PackedBlock
packWholeFiles = undefined

checksum :: PackedBlock -> Int
checksum = sum . zipWith (*) [0 ..] . map (fromMaybe 0)

part1 :: Input -> Int
part1 = checksum . pack

part2 :: Input -> Int
part2 = checksum . packWholeFiles

prepare :: [String] -> Input
prepare = inflate . head

main :: IO ()
main = run part1 part2 prepare
