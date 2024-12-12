module Main where

import Advent.Parser (Parser, runParser)
import Advent.Parser qualified as P
import Advent.Utils (run)
import Control.Applicative (Alternative (..))

type Input = String

data Inst = Do | Dont | Mul Int Int
    deriving (Eq, Show)

testInput :: Input
testInput =
    prepare
        [ "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
        ]

evalMul :: Inst -> Int
evalMul (Mul x y) = x * y
evalMul _ = 0

mulParser :: Parser Char Inst
mulParser = do
    _ <- P.string "mul("
    x <- P.integer
    _ <- P.char ','
    y <- P.integer
    _ <- P.char ')'
    return (Mul x y)

doParser :: Parser Char Inst
doParser = Do <$ P.string "do()"

dontParser :: Parser Char Inst
dontParser = Dont <$ P.string "don't()"

instParser :: Parser Char Inst
instParser = mulParser <|> doParser <|> dontParser

part1 :: Input -> Int
part1 ls = sum $ evalMul <$> findMuls [] ls
  where
    findMuls :: [Inst] -> String -> [Inst]
    findMuls acc [] = acc
    findMuls acc str =
        case runParser mulParser str of
            (Left _) -> findMuls acc (tail str)
            (Right (m, rest)) -> findMuls (m : acc) rest

part2 :: Input -> Int
part2 ls = sum $ evalMul <$> findMuls True [] ls
  where
    findMuls :: Bool -> [Inst] -> String -> [Inst]
    findMuls _ acc [] = acc
    findMuls on acc str =
        case runParser instParser str of
            (Left _) -> findMuls on acc (tail str)
            (Right (m, rest)) -> case m of
                Do -> findMuls True acc rest
                Dont -> findMuls False acc rest
                _ ->
                    if on
                        then findMuls on (m : acc) rest
                        else findMuls on acc rest

prepare :: [String] -> Input
prepare = unlines

main :: IO ()
main = run part1 part2 prepare
