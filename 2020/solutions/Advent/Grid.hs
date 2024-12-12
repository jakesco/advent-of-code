module Advent.Grid (
    Grid,
    Position,
    cardinalNeighborVals,
    cardinalNeighbors,
    columns,
    diagonalNeighborVals,
    diagonalNeighbors,
    dimensions,
    empty,
    findAll,
    findPosition,
    fromList,
    get,
    getAllVals,
    getWithDefault,
    inBounds,
    mapWithPosition,
    neighborVals,
    neighbors,
    rows,
    set,
    toList,
    update,
) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Prelude hiding (pred)

-- | Position in the grid
type Position = (Int, Int)

-- | Grid represented as width, height, and a map from positions to values
data Grid a = Grid
    { width :: Int
    , height :: Int
    , cells :: Map.Map Position a
    }
    deriving (Show, Eq)

instance Functor Grid where
    fmap f (Grid w h cells) =
        Grid w h (fmap f cells)

------------------------------------------------------------------------------

-- | Create an empty grid of given dimensions
empty :: Int -> Int -> Grid a
empty w h = Grid w h Map.empty

-- | Create a grid from a list of lists
fromList :: [[a]] -> Grid a
fromList [] = empty 0 0
fromList rs@(firstRow : _) =
    let
        h = length rs
        w = length firstRow
        positions =
            [ ((x, y), val) | (y, row) <- zip [0 ..] rs, (x, val) <- zip [0 ..] row
            ]
     in
        Grid w h (Map.fromList positions)

-- | Convert grid to list of lists
toList :: Grid a -> [[Maybe a]]
toList g = [[get g (x, y) | x <- [0 .. width g - 1]] | y <- [0 .. height g - 1]]

-- | Get a list of all cell values
getAllVals :: Grid a -> [a]
getAllVals = fmap snd . Map.toList . cells

-- | Check if position is within grid bounds
inBounds :: Grid a -> Position -> Bool
inBounds g (x, y) =
    x >= 0 && x < width g && y >= 0 && y < height g

-- | Get grid dimensions
dimensions :: Grid a -> (Int, Int)
dimensions g = (width g, height g)

-- | Get value at position
get :: Grid a -> Position -> Maybe a
get g pos
    | inBounds g pos = Map.lookup pos (cells g)
    | otherwise = Nothing

-- | Get value at position, return default if not found.
getWithDefault :: Grid a -> a -> Position -> a
getWithDefault g def = fromMaybe def . get g

-- | Set value at position
set :: Grid a -> Position -> a -> Grid a
set g@(Grid w h cells) pos val
    | inBounds g pos = Grid w h (Map.insert pos val cells)
    | otherwise = g

-- | Update value at position using function
update :: Grid a -> Position -> (a -> a) -> Grid a
update g pos f = case get g pos of
    Just val -> set g pos (f val)
    Nothing -> g

-- | Get all eight surrounding positions
neighbors :: Grid a -> Position -> [Position]
neighbors g (x, y) =
    filter
        (inBounds g)
        [ (x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], not (dx == 0 && dy == 0)
        ]

-- | Get only cardinal (non-diagonal) neighbors
cardinalNeighbors :: Grid a -> Position -> [Position]
cardinalNeighbors g (x, y) =
    filter (inBounds g) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- | Get only diagonal neighbors
diagonalNeighbors :: Grid a -> Position -> [Position]
diagonalNeighbors g (x, y) =
    filter (inBounds g) [(x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]

-- | Get all eight surrounding values
neighborVals :: Grid a -> Position -> [a]
neighborVals g pos = mapMaybe (get g) (neighbors g pos)

-- | Get only cardinal (non-diagonal) neighbor values
cardinalNeighborVals :: Grid a -> Position -> [a]
cardinalNeighborVals g pos = mapMaybe (get g) (cardinalNeighbors g pos)

-- | Get only diagonal neighbors
diagonalNeighborVals :: Grid a -> Position -> [a]
diagonalNeighborVals g pos = mapMaybe (get g) (diagonalNeighbors g pos)

-- | Get all rows
rows :: Grid a -> [[Maybe a]]
rows = toList

-- | Get all columns
columns :: Grid a -> [[Maybe a]]
columns g =
    [ [ get g (x, y) | y <- [0 .. height g - 1], x <- [0 .. width g - 1]
      ]
    ]

-- | Map a function that also takes position as input
mapWithPosition :: (Position -> a -> b) -> Grid a -> Grid b
mapWithPosition f (Grid w h cells) =
    Grid w h (Map.mapWithKey f cells)

-- | Find first position of a value satisfying predicate
findPosition :: (a -> Bool) -> Grid a -> Maybe Position
findPosition pred g = listToMaybe $ findAll pred g

-- | Find all positions of values satisfying predicate
findAll :: (a -> Bool) -> Grid a -> [Position]
findAll pred g =
    [ pos
    | pos <-
        [ (x, y) | x <- [0 .. width g - 1], y <- [0 .. height g - 1]
        ]
    , maybe False pred (get g pos)
    ]
