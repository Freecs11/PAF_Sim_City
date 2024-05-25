module AStarTests where

import AStarPathfinding
import qualified Data.Map as Map
import GameData
import Test.Hspec
import Test.QuickCheck
import Zone (getRoutesCoords)
import Control.Monad.State (State, get, put, modify)
import Control.Monad.State (State, evalState)


pathfindingTest :: Spec
pathfindingTest = do
  it "correctly find the shortest path" $ do
    let routes = Route (Rectangle (C 0 0) 10 10)
        zones = Map.fromList [(ZonId 0, routes)]
        ville = Ville Map.empty Map.empty zones
        etat = Etat {ville = ville, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty, selection = None , 
        world = World { worldOffset = C 0 0} , routeDirection = Horizontal, selectionStart = Nothing, pathCache = Map.empty
        , pathfindingQueue = []}
        start = C 1 1
        end = C 5 1
        path =  evalState (aStar start end) etat
    path `shouldBe` Just (4, [C 1 1, C 2 1, C 3 1, C 4 1, C 5 1])

spec :: Spec
spec = do
  pathfindingTest