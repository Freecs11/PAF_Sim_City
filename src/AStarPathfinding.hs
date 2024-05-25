{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AStarPathfinding where

import Control.Monad (guard)
import Data.List (delete, foldl', minimumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import Formes (appartient, adjacent)
import GameData
import Zone (getRoutesCoords)
import Control.Monad.State (State, get, put, modify)


-- A* search algorithm
-- Based on the implementation from  https://notes.abhinavsarkar.net/2022/astar
-- modified to work with the game data structures and also to use cache for memoization


data Node = Node
  { coord :: Coord,
    gCost :: Int,
    hCost :: Int,
    parent :: Maybe Coord
  }
  deriving (Show, Eq)

instance Ord Node where
  compare n1 n2 = compare (gCost n1 + hCost n1) (gCost n2 + hCost n2)

-- Manhattan distance heuristic
heuristic :: Coord -> Coord -> Int
heuristic (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

neighbors :: Coord -> [Coord]
neighbors (C x y) =
  [ C (x + 1) y,
    C (x - 1) y,
    C x (y + 1),
    C x (y - 1),
    C (x + 1) (y + 1),
    C (x - 1) (y - 1),
    C (x + 1) (y - 1),
    C (x - 1) (y + 1)
  ]

getNextNodesAndCost :: Coord -> Etat -> [(Coord, Int)]
getNextNodesAndCost coord etat =
  let neighborsList = neighbors coord
      routes = getRoutesCoords etat
   in foldr (\n acc -> foldr (\r acc' -> if adjacent n r then (n, 1) : acc' else (n, 1005) : acc') acc routes) [] neighborsList

astar ::
  Coord -> Coord -> Etat -> (Coord -> Etat -> [(Coord, Int)]) -> (Coord -> Coord -> Int) -> Maybe (Int, [Coord])
astar startNode goalNode etat nextNodes heuristic =
  astar'
    (PQ.singleton (heuristic startNode goalNode) (startNode, 0))
    Set.empty
    (Map.singleton startNode 0)
    Map.empty
  where
    astar' discovered visited minCosts tracks
      | PQ.null discovered = Nothing
      | node == goalNode = Just (cost, findPath tracks node)
      | node `Set.member` visited = astar' discoveredSansCurrent visited minCosts tracks
      | otherwise =
          let visited' = Set.insert node visited
              successors = [(n, cost', heuristic n goalNode) | (n, nodeCost) <- nextNodes node etat, let cost' = cost + nodeCost, n `Set.notMember` visited', n `Map.notMember` minCosts || cost' < minCosts Map.! n]
              discovered' = foldl' (\q (n, c, h) -> PQ.insert (c + h) (n, c) q) discoveredSansCurrent successors
              minCosts' = foldl' (\m (n, c, _) -> Map.insert n c m) minCosts successors
              tracks' = foldl' (\m (n, _, _) -> Map.insert n node m) tracks successors
           in astar' discovered' visited' minCosts' tracks'
      where
        ((_, (node, cost)), discoveredSansCurrent) = PQ.deleteFindMin discovered

    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (tracks Map.! node) ++ [node]
        else [node]

-- A* search with memoization
aStar :: Coord -> Coord -> State Etat (Maybe (Int, [Coord]))
aStar start goal = do
  state <- get
  let cache = pathCache state
  case Map.lookup (start, goal) cache of
    Just path -> return (Just path) -- Path is cached
    Nothing -> do
      let path = astar start goal state getNextNodesAndCost heuristic
      case path of
        Just p -> do
          modify (\s -> s { pathCache = Map.insert (start, goal) p (pathCache s) })
          return (Just p)
        Nothing -> return Nothing

