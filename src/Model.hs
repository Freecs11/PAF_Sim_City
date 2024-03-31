
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import qualified Data.Map as Map

import qualified GameData as GameData
import GameData (Ville (..), Batiment (..), Coord (..), Forme (..), ZonId (..), BatId (..), CitId (..))


-- A supprimer 
data GameState = GameState {
    city :: Ville,
    coins :: Int,
    currentBuilding :: String
} deriving (Show, Eq)

-- Initialize the game state
initGameState :: GameState
initGameState = GameState {
    city = Ville { viZones = Map.empty, viCit = Map.empty },
    coins = 1000,
    currentBuilding = "cabane"
  }


-- Update the game state
