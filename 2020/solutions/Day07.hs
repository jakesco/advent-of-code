module Main where

import Advent.Parser (Parser, parse)
import Advent.Parser qualified as P
import Advent.Utils (run)
import Control.Applicative (Alternative (..))

import Data.Map (Map)
import Data.Map qualified as Map

type Input = Rules

newtype Bag = Bag String deriving (Show, Eq, Ord)
newtype Rule = Rule (Map Bag Int) deriving (Show, Eq, Ord)
type Rules = Map Bag Rule

testInput :: [String]
testInput =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
    , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    , "bright white bags contain 1 shiny gold bag."
    , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    , "faded blue bags contain no other bags."
    , "dotted black bags contain no other bags."
    ]

shinyGold :: Bag
shinyGold = Bag "shiny gold"

canContain :: Bag -> Rules -> Bag -> Bool
canContain target rules bag =
    case Map.lookup bag rules of
        Nothing -> False
        Just r@(Rule m) ->
            inKeys target r || any (canContain target rules) (Map.keys m)

doesContain :: Bag -> Rules -> Int
doesContain bag rules =
    case Map.lookup bag rules of
        Nothing -> 0
        Just (Rule m) ->
            1 + Map.foldlWithKey (\acc b n -> acc + n * doesContain b rules) 0 m

inKeys :: Bag -> Rule -> Bool
inKeys bag (Rule m) = bag `elem` Map.keys m

part1 :: Input -> Int
part1 rules = length $ filter (canContain shinyGold rules) $ Map.keys rules

part2 :: Input -> Int
part2 rules = doesContain shinyGold rules - 1

bagParser :: Parser Char Bag
bagParser = do
    x <- P.word
    _ <- P.space
    y <- P.word
    _ <- P.space
    _ <- P.string "bags" <|> P.string "bag"
    return $ Bag (x <> " " <> y)

ruleParser :: Parser Char (Bag, Rule)
ruleParser = do
    b <- bagParser
    _ <- P.space *> P.string "contain" <* P.space
    r <- ruleParser'
    return (b, r)
  where
    ruleParser' = Rule . Map.fromList <$> P.sepBy rule (P.char ',' *> P.space)
      where
        rule = do
            x <- P.integer
            _ <- P.space
            bag <- bagParser
            return (bag, x)

parseRule :: String -> (Bag, Rule)
parseRule s = case parse ruleParser s of
    (Left _) -> error "failed to parse rule"
    (Right r) -> fst r

prepare :: String -> Input
prepare = Map.fromList . map parseRule . lines

main :: IO ()
main = run part1 part2 prepare
