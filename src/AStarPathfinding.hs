module AStarPathfinding where

import Data.List (minimumBy, delete, foldl')
import Data.Ord (comparing)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (guard)

import GameData

data Node = Node
    { coord :: Coord
    , gCost :: Int
    , hCost :: Int
    , parent :: Maybe Coord
    } deriving (Show, Eq)

instance Ord Node where
    compare n1 n2 = compare (gCost n1 + hCost n1) (gCost n2 + hCost n2)

-- Manhattan distance heuristic
heuristic :: Coord -> Coord -> Int
heuristic (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

neighbors :: Coord -> [Coord]
neighbors (C x y) =
    [ C (x + 1) y
    , C (x - 1) y
    , C x (y + 1)
    , C x (y - 1)
    ]

aStar :: Set.Set Coord -> Coord -> Coord -> Maybe [Coord]
aStar obstacles start goal = aStar' (Map.singleton start (Node start 0 (heuristic start goal) Nothing)) Set.empty
  where
    aStar' openSet closedSet
        | Map.null openSet = Nothing
        | currentCoord == goal = Just $ reconstructPath currentNode openSet
        | otherwise = aStar' openSet'' closedSet'
      where
        currentNode@(Node currentCoord _ _ _) = minimumBy (comparing (\n -> gCost n + hCost n)) (Map.elems openSet)
        openSet' = Map.delete currentCoord openSet
        closedSet' = Set.insert currentCoord closedSet
        openSet'' = foldl' (expandNode currentNode closedSet') openSet' (neighbors currentCoord)
        
    expandNode currentNode closedSet openSet neighbor
        | Set.member neighbor closedSet = openSet
        | otherwise =
            let tentativeG = gCost currentNode + 1
                neighborNode = Map.lookup neighbor openSet
                newNode = case neighborNode of
                    Just n
                        | tentativeG < gCost n -> n { gCost = tentativeG, parent = Just (coord currentNode) }
                    Nothing -> Node neighbor tentativeG (heuristic neighbor goal) (Just (coord currentNode))
                    Just n -> n
            in Map.insert neighbor newNode openSet

    reconstructPath (Node coord _ _ Nothing) _ = [coord]
    reconstructPath (Node coord _ _ (Just parentCoord)) openSet =
        coord : reconstructPath (openSet Map.! parentCoord) openSet
