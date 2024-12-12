module Main where

import Advent.Utils (run)
import Control.Monad (msum)
import Data.List (unfoldr)

-- TODO: May be worth switching to Array
type Input = [Op]

data Op
    = Jmp Int
    | Acc Int
    | Nop Int
    deriving (Show, Eq)

data State = State
    { pc :: Int
    , acc :: Int
    , visited :: [Int]
    }
    deriving (Show)

testInput :: [String]
testInput =
    [ "nop +0"
    , "acc +1"
    , "jmp +4"
    , "acc +3"
    , "jmp -3"
    , "acc -99"
    , "acc +1"
    , "jmp -4"
    , "acc +6"
    ]

runOp :: Op -> State -> State
runOp (Nop _) s@(State pc _ v) =
    s{pc = pc + 1, visited = pc : v}
runOp (Jmp n) s@(State pc _ v) =
    s{pc = pc + n, visited = pc : v}
runOp (Acc n) s@(State pc acc v) =
    s{pc = pc + 1, acc = acc + n, visited = pc : v}

swapOps :: Input -> [Input]
swapOps ops =
    unfoldr
        ( \i ->
            let
                nextI = findNextSwap i ops
             in
                if nextI >= length ops
                    then Nothing
                    else Just (swapOp nextI ops, nextI + 1)
        )
        0
  where
    findNextSwap :: Int -> Input -> Int
    findNextSwap i ops'
        | i >= length ops' = i
        | isAcc (ops' !! i) = findNextSwap (i + 1) ops'
        | otherwise = i

    isAcc :: Op -> Bool
    isAcc (Acc _) = True
    isAcc _ = False

swapOp :: Int -> Input -> Input
swapOp i ops = case splitAt i ops of
    (_, []) -> ops
    (a, (Jmp n) : rest) ->
        a ++ (Nop n : rest)
    (a, (Nop n) : rest) ->
        a ++ (Jmp n : rest)
    _ -> ops

parseOp :: String -> Op
parseOp s =
    case words s of
        ["nop", n] -> Nop (parseInt n)
        ["acc", n] -> Acc (parseInt n)
        ["jmp", n] -> Jmp (parseInt n)
        _ -> error ("Faild to parse op: " <> s)
  where
    parseInt i
        | head i == '+' = read (tail i)
        | otherwise = read i

part1 :: Input -> Int
part1 ops = acc $ runProgram ops (State 0 0 [])
  where
    runProgram :: Input -> State -> State
    runProgram ops' s@(State p _ v) =
        if p `elem` v
            then s
            else runProgram ops' (runOp (ops' !! pc s) s)

part2 :: Input -> Int
part2 ops = case msum $ map (runProgram (State 0 0 [])) $ swapOps ops of
    Nothing -> error "No swaps found."
    (Just s) -> acc s
  where
    runProgram :: State -> Input -> Maybe State
    runProgram s@(State p _ v) ops'
        | p `elem` v = Nothing
        | p >= length ops' = Just s
        | otherwise = runProgram (runOp (ops' !! pc s) s) ops'

prepare :: String -> Input
prepare = map parseOp . lines

main :: IO ()
main = run part1 part2 prepare
