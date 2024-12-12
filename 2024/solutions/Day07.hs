module Main where

import Advent.Utils (run)
import Control.Monad (replicateM)
import Data.List.Split (splitOn)

type Input = [Calibration]
data Op = Add | Mul | Concat deriving (Eq, Show)
data Calibration = Calibration Int [Int]
    deriving (Show)

testInput :: Input
testInput =
    prepare
        [ "190: 10 19"
        , "3267: 81 40 27"
        , "83: 17 5"
        , "156: 15 6"
        , "7290: 6 8 6 15"
        , "161011: 16 10 13"
        , "192: 17 8 14"
        , "21037: 9 7 18 13"
        , "292: 11 6 16 20"
        ]

concatInts :: Int -> Int -> Int
concatInts x y = read $ show x <> show y

checkCalibration :: [Op] -> Calibration -> Int
checkCalibration ops cal@(Calibration _ ls) = checkCalibration' cal (replicateM (length ls - 1) ops)
  where
    checkCalibration' _ [] = 0
    checkCalibration' cal'@(Calibration x xs) (ys : rest) =
        if eval xs ys == x then x else checkCalibration' cal' rest

    eval :: [Int] -> [Op] -> Int
    eval xs fs =
        foldl
            ( \acc (x, f) -> case f of
                Add -> acc + x
                Mul -> acc * x
                Concat -> concatInts acc x
            )
            (head xs)
            (zip (tail xs) fs)

part1 :: Input -> Int
part1 cals = sum $ checkCalibration [Add, Mul] <$> cals

part2 :: Input -> Int
part2 cals = sum $ checkCalibration [Add, Mul, Concat] <$> cals

prepare :: [String] -> Input
prepare = map (parseCalibration . splitOn ":")
  where
    parseCalibration (x : y : _) = Calibration (read x) (read <$> words y)
    parseCalibration x = error ("Unknown input: " <> show x)

main :: IO ()
main = run part1 part2 prepare
