module Batiments where

import GameData
import Sprite
import SDL
import TextureMap
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import Data.Sequence (Seq (..) )
import qualified Data.Sequence as Seq
import SDL.Vect (V2 (..), Point (..))
import SDL.Video.Renderer (Renderer, Texture, Rectangle )
import qualified SDL.Video.Renderer as R
import Data.Text (Text)
import Data.Word (Word8)
import qualified SDL
import SDL (V4(..), Point(..), Rectangle(..) )
import qualified SDL.Font as Font
import qualified Debug.Trace as T
import qualified Data.Map as Map
import Data.Map (Map)

-- TOCHANGE

-- get the map of buildings from a city
getBatiments :: Ville -> Map BatId Batiment
getBatiments Ville { viZones = zones , viBat = batiments} = batiments

-- check if a building is at a coordinate
isBatimentAt :: Coord -> BatId -> Etat -> Bool
isBatimentAt coord batId (Etat {ville = ville}) = let batiments = getBatiments ville
    in case Map.lookup batId batiments of
        Just batiment -> case batiment of
            Cabane forme coord' _ _ -> coord == coord'
            Atelier forme coord' _ _ -> coord == coord'
            Epicerie forme coord' _ _ -> coord == coord'
            Commissariat forme coord' -> coord == coord'
        Nothing -> False

-- verify if the Coord and BatId of the state are coherent
-- meaning that the building at the coord in Carte has the same batId
-- as the one in the state
prop_carte_coords_valid_Ville :: Etat -> Bool
prop_carte_coords_valid_Ville etat@(Etat {ville = ville, carte = carte}) = Map.foldrWithKey step True carte
    where
        step :: Coord -> (BatId, [CitId]) -> Bool -> Bool
        step coord (batId, _) acc = acc && isBatimentAt coord batId etat
        
-- check if map has a building at a coordinate
hasBatimentAt :: BatId -> Etat -> Bool
hasBatimentAt batId etat@(Etat {ville = ville , carte =c }) = let batiments = getBatiments ville
    in case Map.lookup batId batiments of
        Just bat -> case bat of
            Cabane _ coord _ _ -> lookUpCarteWithBatID coord etat batId
            Atelier _ coord _ _ -> lookUpCarteWithBatID coord etat batId
            Epicerie _ coord _ _ -> lookUpCarteWithBatID coord etat batId
            Commissariat _ coord -> lookUpCarteWithBatID coord etat batId
        Nothing -> False

-- lookup if the building at a coordinate in the carte match the batId 
lookUpCarteWithBatID :: Coord -> Etat -> BatId -> Bool
lookUpCarteWithBatID coord (Etat {carte = carte}) batId = case Map.lookup coord carte of
    Just (batId', _) -> batId == batId'
    Nothing -> False

-- verify if all the buildings have coherent coords with the carte of the state
-- meaning that the carte has the same batId as the building at the same coord
prop_carteCoordBat_inv :: Etat -> Bool
prop_carteCoordBat_inv etat@(Etat {ville = ville, carte = carte}) = let batiments = getBatiments ville
    in Map.foldrWithKey step True batiments
    where
        step :: BatId -> Batiment -> Bool -> Bool
        step batId batiment acc = acc && hasBatimentAt batId etat


-- smaert constructor 
-- create a building with a unique id
createBatiment :: BatId -> Batiment -> Etat -> Etat
createBatiment batId batiment etat@(Etat {ville = ville}) =
    let batiments = Map.insert batId batiment $ getBatiments ville
    in etat {ville = ville {viBat = batiments}
    , carte = insertBatimentInCarte batId etat
    }

    
-- remove a building from the city
removeBatiment :: BatId -> Etat -> Etat
removeBatiment batId etat@(Etat {ville = ville}) = 
    let batiments = Map.delete batId $ getBatiments ville
    in etat {ville = ville {viBat = batiments}
    , carte = removeBatimentFromCarte batId etat
    }

-- insert a building in the carte
insertBatimentInCarte :: BatId -> Etat -> Map Coord (BatId, [CitId])
insertBatimentInCarte batId etat@(Etat {ville = ville, carte = carte}) = 
    let maybeCoord = getCoordBatiment batId etat
    in case maybeCoord of
        Just coord -> case Map.lookup coord carte of
            Just (batId', citIds) -> 
                if batId' == batId then carte
                else Map.insert coord (batId, citIds) carte -- à changer , là on écrase les batiments qui sont déjà à cette coordonnée
            Nothing -> Map.insert coord (batId, []) carte
        Nothing -> carte


-- remove a building from the carte same way as removeCitFromCarte
removeBatimentFromCarte :: BatId -> Etat -> Map Coord (BatId, [CitId])
removeBatimentFromCarte batId etat@(Etat {carte = carte}) = 
    let maybeCoord = getCoordBatiment batId etat
    in case maybeCoord of
        Just coord -> case Map.lookup coord carte of
            Just (batId', citIds) -> 
                if batId' == batId then Map.delete coord carte
                else carte
            Nothing -> carte
        Nothing -> carte

-- get the coordinates of a building
getCoordBatiment :: BatId -> Etat -> Maybe Coord
getCoordBatiment batId (Etat {ville = ville}) = let batiments = getBatiments ville
    in case Map.lookup batId batiments of
        Just bat -> case bat of
            Cabane _ coord _ _ -> Just coord
            Atelier _ coord _ _ -> Just coord
            Epicerie _ coord _ _ -> Just coord
            Commissariat _ coord -> Just coord

        Nothing -> Nothing

-- get the building at a coordinate
getBatimentAt :: Coord -> Etat -> Maybe Batiment
getBatimentAt coord (Etat {ville = ville}) = let batiments = getBatiments ville
    in case Map.foldrWithKey step Nothing batiments of
        Just bat -> Just bat
        Nothing -> Nothing
    where
        step :: BatId -> Batiment -> Maybe Batiment -> Maybe Batiment
        step batId bat acc = case bat of
            Cabane _ coord' _ _ -> if coord == coord' then Just bat else acc
            Atelier _ coord' _ _ -> if coord == coord' then Just bat else acc
            Epicerie _ coord' _ _ -> if coord == coord' then Just bat else acc
            Commissariat _ coord' -> if coord == coord' then Just bat else acc
            _ -> acc

-- get the last batId in the city
getLastBatId :: Etat -> BatId
getLastBatId (Etat {ville = ville}) = let batiments = getBatiments ville
    in case Map.foldrWithKey step Nothing batiments of
        Just batId -> batId
        Nothing -> BatId 0
    where
        step :: BatId -> Batiment -> Maybe BatId -> Maybe BatId
        step batId _ acc = case acc of
            Just acc' -> if batId > acc' then Just batId else acc
            Nothing -> Just batId
            




