module Main where

import Advent.Utils (run)

import Prelude hiding (max, min)

type Input = [Password]

data Password = Password
  { content :: String
  , min :: Int
  , max :: Int
  , char :: Char
  }
  deriving (Show)

part1 :: Input -> Int
part1 = length . filter pValid
 where
  pValid :: Password -> Bool
  pValid p =
    let
      c = countChars (char p) $ content p
     in
      (c >= min p) && (c <= max p)

  countChars :: Char -> String -> Int
  countChars needle =
    foldl (\acc c -> if c == needle then acc + 1 else acc) 0

part2 :: Input -> Int
part2 = length . filter pValid
 where
  pValid :: Password -> Bool
  pValid p = (first == needle) /= (second == needle)
   where
    needle = char p
    first = content p !! (min p - 1)
    second = content p !! (max p - 1)

prepare :: String -> Input
prepare = map parsePassword . lines
 where
  parsePassword :: String -> Password
  parsePassword s =
    let
      [range, needle, password] = words s
      (m', n') = break (== '-') range
      m = read m' :: Int
      n = read $ tail n' :: Int
     in
      Password{content = password, min = m, max = n, char = head needle}

main :: IO ()
main = run part1 part2 prepare
