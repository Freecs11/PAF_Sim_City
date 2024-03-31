module State where


-- state of the game (citizens, buildings, zones, etc.) 
-- we'll serve for simulation too

import GameData

import Data.Map (Map)
import qualified Data.Map as Map


data Etat =  Etat {
        ville :: Ville,
        coins :: Int
    }
    deriving (Show, Eq)


-- | initial state of the game
-- initEtat :: Etat
-- initEtat = Etat initVille 1000

-- -- | update the state of the game
-- updateEtat :: Etat -> Etat
-- updateEtat (Etat v c) = Etat (updateVille v) c


