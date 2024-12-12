module Main where

import Advent.Grid (Grid, Position)
import Advent.Grid qualified as Grid
import Advent.Utils (run)
import Data.List (nub, unfoldr)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

import Debug.Trace (traceShow)

type Input = Grid Char
data SimStatus = Exit | Loop | Running
    deriving (Eq, Show)
data Facing = North | South | East | West
    deriving (Eq, Ord, Show)
data Guard = Guard Position Facing
    deriving (Eq, Ord, Show)
data State = State
    { current :: Guard
    , visited :: Set Guard
    , status :: SimStatus
    , steps :: Int
    }
    deriving (Eq, Show)

testInput :: Input
testInput =
    prepare
        [ "....#....."
        , ".........#"
        , ".........."
        , "..#......."
        , ".......#.."
        , ".........."
        , ".#..^....."
        , "........#."
        , "#........."
        , "......#..."
        ]

nextPos :: Guard -> Position
nextPos (Guard (x, y) North) = (x, y - 1)
nextPos (Guard (x, y) South) = (x, y + 1)
nextPos (Guard (x, y) East) = (x + 1, y)
nextPos (Guard (x, y) West) = (x - 1, y)

turn :: Guard -> Guard
turn (Guard pos North) = Guard pos East
turn (Guard pos South) = Guard pos West
turn (Guard pos East) = Guard pos South
turn (Guard pos West) = Guard pos North

move :: Guard -> Guard
move g@(Guard _ facing) = Guard (nextPos g) facing

peak :: Input -> Guard -> Maybe Char
peak grid guard = Grid.get grid $ nextPos guard

initialState :: Input -> State
initialState grid =
    let
        start = Grid.findPosition (== '^') grid
     in
        State (Guard (fromJust start) North) Set.empty Running 0

countVisited :: State -> Int
countVisited state = length . nub $ (\(Guard pos _) -> pos) <$> Set.elems (visited state)

-- | Gives the next state after one update
runStep :: Input -> State -> State
runStep grid state = go (peak grid (current state))
  where
    go :: Maybe Char -> State
    go Nothing = state{status = Exit, steps = steps state + 1}
    go (Just '#') = state{current = turn (current state), steps = steps state + 1}
    go (Just _) =
        let
            nextGuard = move (current state)
            nextVisited = Set.insert (current state) (visited state)
         in
            -- TODO: I'm over classifying Loops
            if Set.member nextGuard (visited state)
                then state{status = Loop}
                else State nextGuard nextVisited Running (steps state + 1)

-- | Runs steps until either Loop or Exit
runSimulation :: Input -> State -> [State]
runSimulation grid =
    unfoldr
        ( \s@(State _ _ status' _) ->
            if status' == Running
                then Just (s, runStep grid s)
                else Nothing
        )

{- | Similar to run simulation, but doesn't accumulate state list
  sets point in front of guard to '#'.
-}
findLoop :: Input -> State -> Bool
findLoop grid state =
    let
        nextPoint = nextPos (current state)
        nextGrid = Grid.set grid nextPoint '#'
        result = go nextGrid state
     in
        traceShow (current state, steps state, result) result
  where
    go :: Input -> State -> Bool
    go _ (State _ _ Loop _) = True
    go _ (State _ _ Exit _) = False
    go g s =
        -- NOTE: Sim was getting stuck at a certain test case, this will
        -- kick it out if it seems to be going too long
        (steps s <= uncurry (*) (Grid.dimensions g)) && go g (runStep g s)

part1 :: Input -> Int
part1 grid = (1 +) . countVisited . last $ runSimulation grid (initialState grid)

-- | Uses results from part1 to inform search space
part2 :: Input -> Int
part2 grid =
    let
        searchSpace = runSimulation grid (initialState grid)
     in
        traceShow (length searchSpace) length $ filter (findLoop grid) searchSpace

prepare :: [String] -> Input
prepare = Grid.fromList

main :: IO ()
main = run part1 part2 prepare
