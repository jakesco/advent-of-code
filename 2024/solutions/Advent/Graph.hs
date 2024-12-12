module Advent.Graph (
    Graph,
    empty,
    addVertex,
    addDirectedEdge,
    removeVertex,
    removeEdge,
    vertices,
    edges,
    outNeighbors,
    inNeighbors,
    bfs,
    dfs,
    hasPath,
    shortestPath,
    hasCycle,
) where

import Data.Map qualified as Map
import Data.Set qualified as Set

-- | Directed Graph with both outgoing and incoming edges
data Graph a = Graph
    { outgoing :: Map.Map a (Set.Set a)
    , incoming :: Map.Map a (Set.Set a)
    }
    deriving (Show, Eq)

------------------------------------------------------------------------------

-- | Create an empty graph
empty :: Graph a
empty = Graph Map.empty Map.empty

{- | Add a vertex to the graph
  nop if the vertex already exists
-}
addVertex :: (Ord a) => a -> Graph a -> Graph a
addVertex v g@(Graph o i) =
    if v `elem` vertices g
        then g
        else
            Graph (Map.insert v Set.empty o) (Map.insert v Set.empty i)

-- | Add a directed edge from v1 to v2
addDirectedEdge :: (Ord a) => a -> a -> Graph a -> Graph a
addDirectedEdge v1 v2 (Graph o i) =
    Graph
        (Map.adjust (Set.insert v2) v1 o)
        (Map.adjust (Set.insert v1) v2 i)

-- | Remove a vertex and all its edges
removeVertex :: (Ord a) => a -> Graph a -> Graph a
removeVertex v (Graph o i) =
    let
        o' = Map.map (Set.delete v) $ Map.delete v o
        i' = Map.map (Set.delete v) $ Map.delete v i
     in
        Graph o' i'

-- | Remove a directed edge from v1 to v2
removeEdge :: (Ord a) => a -> a -> Graph a -> Graph a
removeEdge v1 v2 (Graph o i) =
    Graph
        (Map.adjust (Set.delete v2) v1 o)
        (Map.adjust (Set.delete v1) v2 i)

-- | Get all vertices in the graph
vertices :: Graph a -> [a]
vertices (Graph o _) = Map.keys o

-- | Get all directed edges in the graph
edges :: (Ord a) => Graph a -> [(a, a)]
edges (Graph o _) =
    [(v1, v2) | (v1, ns) <- Map.toList o, v2 <- Set.toList ns]

-- | Get vertices that this vertex points to
outNeighbors :: (Ord a) => a -> Graph a -> [a]
outNeighbors v (Graph o _) = maybe [] Set.toList $ Map.lookup v o

-- | Get vertices that point to this vertex
inNeighbors :: (Ord a) => a -> Graph a -> [a]
inNeighbors v (Graph _ i) = maybe [] Set.toList $ Map.lookup v i

-- | Breadth-First Search (following directed edges)
bfs :: (Ord a) => a -> Graph a -> [a]
bfs = undefined

-- | Depth-First Search (following directed edges)
dfs :: (Ord a) => a -> Graph a -> [a]
dfs = undefined

-- | Check if there's a directed path between two vertices
hasPath :: (Ord a) => a -> a -> Graph a -> Bool
hasPath = undefined

-- | Find shortest directed path between two vertices
shortestPath :: (Ord a) => a -> a -> Graph a -> Maybe [a]
shortestPath = undefined

-- | Detect if the graph has a cycle
hasCycle :: (Ord a) => Graph a -> Bool
hasCycle = undefined
