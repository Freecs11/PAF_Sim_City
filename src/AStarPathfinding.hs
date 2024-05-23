{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AStarPathfinding where

import Control.Monad (guard)
import Data.List (delete, foldl', minimumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import Formes (appartient)
import GameData
import Zone (getRoutesCoords)

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
    C x (y - 1)
  ]

getNextNodesAndCost :: Coord -> Etat -> [(Coord, Int)]
getNextNodesAndCost coord etat =
  let neighborsList = neighbors coord
      routes = getRoutesCoords etat
   in foldr (\n acc -> foldr (\r acc' -> if appartient n r then (n, 1) : acc' else acc') acc routes) [] neighborsList

-- aStar :: Set.Set Coord -> Coord -> Coord -> Maybe [Coord]
-- aStar obstacles start goal = aStar' (Map.singleton start (Node start 0 (heuristic start goal) Nothing)) Set.empty
--   where
--     aStar' openSet closedSet
--       | Map.null openSet = Nothing
--       | currentCoord == goal = Just $ reconstructPath currentNode openSet
--       | otherwise = aStar' openSet'' closedSet'
--       where
--         currentNode@(Node currentCoord _ _ _) = minimumBy (comparing (\n -> gCost n + hCost n)) (Map.elems openSet)
--         openSet' = Map.delete currentCoord openSet
--         closedSet' = Set.insert currentCoord closedSet
--         openSet'' = foldl' (expandNode currentNode closedSet') openSet' (neighbors currentCoord)

--     expandNode currentNode closedSet openSet neighbor
--       | Set.member neighbor closedSet = openSet
--       | otherwise =
--           let tentativeG = gCost currentNode + 1
--               neighborNode = Map.lookup neighbor openSet
--               newNode = case neighborNode of
--                 Just n
--                   | tentativeG < gCost n -> n {gCost = tentativeG, parent = Just (coord currentNode)}
--                 Nothing -> Node neighbor tentativeG (heuristic neighbor goal) (Just (coord currentNode))
--                 Just n -> n
--            in Map.insert neighbor newNode openSet

--     reconstructPath (Node coord _ _ Nothing) _ = [coord]
--     reconstructPath (Node coord _ _ (Just parentCoord)) openSet =
--       coord : reconstructPath (openSet Map.! parentCoord) openSet

-- Source: https://notes.abhinavsarkar.net/2022/astar
astar ::
  -- | The start node.
  Coord ->
  -- | The goal node.
  Coord ->
  -- | The initial state.
  Etat ->
  -- | The function to get the next nodes and their costs from a given node.
  (Coord -> Etat -> [(Coord, Int)]) ->
  -- | The heuristic function to estimate the cost of going from a given node to
  --   the goal node.
  (Coord -> Coord -> Int) ->
  -- | Returns Nothing if no path found.
  --   Else returns Just (path cost, path as a list of nodes).
  Maybe (Int, [Coord])
astar startNode goalNode etat nextNodes heuristic =
  astar'
    (PQ.singleton (heuristic startNode goalNode) (startNode, 0))
    Set.empty
    (Map.singleton startNode 0)
    Map.empty
  where
    astar' ::
      -- \| The set of discovered nodes that need to be visited, stored
      --   in a min-priority queue prioritized by sum of costs of reaching to
      --   the nodes from the start node, and heuristic costs of reaching
      --   from the nodes to the goal node.
      PQ.MinPQueue Int (Coord, Int) ->
      -- \| The set of already visited nodes.
      Set.Set Coord ->
      -- \| The map of visited or discovered nodes to the currently known minimum
      --   costs from the start node to the nodes.
      Map.Map Coord Int ->
      -- \| The map of visited nodes to the previous nodes in the currently known
      --   best path from the start node.
      Map.Map Coord Coord ->
      -- \| Returns Nothing if no path found.
      --   Else returns Just (path cost, path as a list of nodes).
      Maybe (Int, [Coord])
    astar' !discovered !visited !minCosts tracks
      -- If the discovered set is empty then the search has failed. Return Nothing.
      | PQ.null discovered = Nothing
      -- If the current node is the goal node then return the current node cost and
      -- path to the current node constructed from the tracks.
      | node == goalNode = Just (cost, findPath tracks node)
      -- If the current node has already been visited then discard it and continue.
      | node `Set.member` visited =
          astar' discoveredSansCurrent visited minCosts tracks
      -- Else visit the current node and continue.
      | otherwise =
          let -- Add the current node to the visited set.
              visited' = Set.insert node visited
              -- Find the successor nodes of the current node that have not been
              -- visited yet, along with their costs and heuristic costs.
              successors =
                [ (node', cost', heuristic node' goalNode)
                  | (node', nodeCost) <- nextNodes node etat, -- Get next nodes.
                    node' `Set.notMember` visited', -- Keep only unvisited ones.
                    let cost' = cost + nodeCost, -- Cost of the next node.
                    -- Keep only unvisited nodes, or previously visited nodes now
                    -- discovered via less costly paths.
                    node' `Map.notMember` minCosts || cost' < minCosts Map.! node'
                ]

              -- Insert the successors in the discovered set.
              discovered' =
                foldl'
                  (\q (n, c, h) -> PQ.insert (c + h) (n, c) q)
                  discoveredSansCurrent
                  successors
              -- Insert the successor costs in the minimum cost map.
              minCosts' = foldl' (\m (n, c, _) -> Map.insert n c m) minCosts successors
              -- Insert the tracks of the successors.
              tracks' = foldl' (\m (n, _, _) -> Map.insert n node m) tracks successors
           in -- Continue via recursion.
              astar' discovered' visited' minCosts' tracks'
      where
        -- Get (and delete) the node with minimum cost and its cost from the
        -- discovered set.
        ((_, (node, cost)), discoveredSansCurrent) = PQ.deleteFindMin discovered

    -- Construct the path of the given node from the start node using the
    -- recorded tracks.
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (tracks Map.! node) ++ [node]
        else [node]

aStar :: Coord -> Coord -> Etat -> Maybe (Int, [Coord])
aStar start goal etat = astar start goal etat getNextNodesAndCost heuristic