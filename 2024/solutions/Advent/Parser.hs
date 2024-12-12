module Advent.Parser (
    Parser,
    between,
    char,
    integer,
    runParser,
    satisfy,
    sepBy,
    sepBy1,
    space,
    spaces,
    string,
    word,
) where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit, isLetter, isSpace)
import Data.List (nub)

data Error i
    = EndOfInput
    | Unexpected i
    | Empty
    deriving (Eq, Show)

newtype Parser i a = Parser
    { runParser :: [i] -> Either [Error i] (a, [i])
    }

instance Functor (Parser i) where
    fmap f (Parser p) = Parser $ \input ->
        case p input of
            Left err -> Left err
            Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i) where
    pure a = Parser $ \input -> Right (a, input)
    Parser f <*> Parser p = Parser $ \input ->
        case f input of
            Left err -> Left err
            Right (f', rest) ->
                case p rest of
                    Left err -> Left err
                    Right (output, rest') -> Right (f' output, rest')

instance Monad (Parser i) where
    return = pure

    Parser p >>= k = Parser $ \input ->
        case p input of
            Left err -> Left err
            Right (output, rest) ->
                let
                    Parser p' = k output
                 in
                    p' rest

instance (Eq i) => Alternative (Parser i) where
    empty = Parser $ \_ -> Left [Empty]

    Parser l <|> Parser r = Parser $ \input ->
        case l input of
            Left err ->
                case r input of
                    Left err' -> Left $ nub $ err <> err'
                    Right (output, rest) -> Right (output, rest)
            Right (output, rest) -> Right (output, rest)

satisfy :: (i -> Bool) -> Parser i i
satisfy predicate = Parser $ \input ->
    case input of
        [] -> Left [EndOfInput]
        hd : rest
            | predicate hd -> Right (hd, rest)
            | otherwise -> Left [Unexpected hd]

char :: Char -> Parser Char Char
char i = satisfy (== i)

string :: String -> Parser Char String
string [] = pure []
string (x : xs) = (:) <$> char x <*> string xs

space :: Parser Char Char
space = satisfy isSpace

spaces :: Parser Char String
spaces = many space

between :: Parser i b -> Parser i a -> Parser i c -> Parser i a
between open p close = open *> p <* close

sepBy :: (Eq i) => Parser i a -> Parser i sep -> Parser i [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: (Eq i) => Parser i a -> Parser i sep -> Parser i [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

word :: Parser Char String
word = some (satisfy isLetter)

integer :: Parser Char Int
integer = read <$> (negative <|> num)
  where
    negative = (:) <$> char '-' <*> num
    num = some (satisfy isDigit)
