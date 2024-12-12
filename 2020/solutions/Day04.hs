module Main where

import Advent.Utils (run)
import Data.Char (isDigit, isHexDigit)
import Data.List (intersect, partition)

type Passport = String
type Input = [Passport]

requiredKeys :: [String]
requiredKeys =
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
    ]

testInput :: Input
testInput =
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    , "byr:1937 iyr:2017 cid:147 hgt:183cm"
    , ""
    , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    , "hcl:#cfa07d byr:1929"
    , ""
    , "hcl:#ae17e1 iyr:2013"
    , "eyr:2024"
    , "ecl:brn pid:760753108 byr:1931"
    , "hgt:179cm"
    , ""
    , "hcl:#cfa07d eyr:2025 pid:166559648"
    , "iyr:2011 ecl:brn hgt:59in"
    ]

keysPresent :: Passport -> Bool
keysPresent p = length (intersect requiredKeys $ getKeys p) == 7
  where
    getKeys p' = map (takeWhile (/= ':')) (words p')

splitOn :: (Char -> Bool) -> String -> (String, String)
splitOn f s = (h, tail t) where (h, t) = break f s

getKeyVals :: Passport -> [(String, String)]
getKeyVals p = map (splitOn (== ':')) $ words p

parseInt :: String -> Int
parseInt = read

parseHgt :: String -> (Int, String)
parseHgt s = let (n, t) = partition isDigit s in (read n :: Int, t)

validate :: (String, String) -> Bool
validate (key, val) = case key of
    "byr" -> let year = parseInt val in year >= 1920 && year <= 2002
    "iyr" -> let year = parseInt val in year >= 2010 && year <= 2020
    "eyr" -> let year = parseInt val in year >= 2020 && year <= 2030
    "hgt" -> case parseHgt val of
        (n, "cm") -> n >= 150 && n <= 193
        (n, "in") -> n >= 59 && n <= 76
        _ -> False
    "hcl" -> head val == '#' && all isHexDigit (tail val)
    "ecl" -> val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    "pid" -> length val == 9 && all isDigit val
    "cid" -> True
    _ -> False

passportValid :: Passport -> Bool
passportValid p = keysPresent p && all validate (getKeyVals p)

part1 :: Input -> Int
part1 = length . filter keysPresent

part2 :: Input -> Int
part2 = length . filter passportValid

collectPassports :: Input -> [Passport]
collectPassports = go []
  where
    go acc [] = acc
    go acc ls = case break null ls of
        (chunk, []) -> unwords chunk : acc
        ([], rest) -> go acc (tail rest)
        (chunk, rest) -> go (unwords chunk : acc) (tail rest)

prepare :: String -> Input
prepare = collectPassports . lines

main :: IO ()
main = run part1 part2 prepare
