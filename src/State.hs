module State where


-- state of the game (citizens, buildings, zones, etc.) 
-- we'll serve for simulation too

import GameData

import Data.Map (Map)
import qualified Data.Map as Map


data Etat =  Etat {
        ville :: Ville,
        coins :: Int,
        carte :: Map Coord (BatId, CitId)
    }
    deriving (Show, Eq)

-- invariant test on Etat
prop_carte_inv :: Etat -> Bool
prop_carte_inv etat  


