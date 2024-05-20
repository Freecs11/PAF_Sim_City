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

import Formes

-- TOCHANGE



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


-- -- smaert constructor 
createBatiment :: Batiment -> Etat -> Etat
createBatiment batiment etat@(Etat {ville = ville}) = 
    case isBatimentValid batiment etat of
        True -> let batiments = getBatiments ville
                    batId = BatId (Map.size batiments)
                    updatedBatiments = Map.insert batId batiment batiments
                in etat {ville = ville {viBat = updatedBatiments}}
        False -> etat

-- buildings are valid if they are not already in the city
-- and if they are disjoint from the buildings already in the city and also are in a zone that is a ZR, ZI or ZC
isBatimentValid :: Batiment -> Etat -> Bool
isBatimentValid batiment etat@(Etat {ville = ville}) = let batiments = getBatiments ville
    in case batiment of
        Cabane forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Atelier forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Epicerie forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Commissariat forme coord -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat

-- check if a building is disjoint from the other buildings
isBatimentDisjoint :: Forme -> Map BatId Batiment -> Bool
isBatimentDisjoint forme batiments = 
    let coords = getFormeCoord forme in 
        foldr (\batiment acc -> case batiment of
            Cabane forme' coord _ _ -> acc && not (appartient coord forme)
            Atelier forme' coord _ _ -> acc && not (appartient coord forme)
            Epicerie forme' coord _ _ -> acc && not (appartient coord forme)
            Commissariat forme' coord -> acc && not (appartient coord forme)
        ) True (Map.elems batiments)



-- check if a building is in a zone that is a ZR, ZI or ZC
isBatimentInZone :: Batiment -> Etat -> Bool
isBatimentInZone batiment etat@(Etat {ville = ville}) = let zones = getZones ville
    in 
        foldr (\zone acc -> case zone of
            ZR forme _ -> acc || isBatimentInZone' forme batiment
            ZI forme _ -> acc || isBatimentInZone' forme batiment
            ZC forme _ -> acc || isBatimentInZone' forme batiment
            _ -> acc
        ) False (Map.elems zones)

-- check if a building is in a zone
isBatimentInZone' :: Forme -> Batiment -> Bool
isBatimentInZone' forme batiment = appartient (getBatimentCoord batiment) forme

-- get the coordinates of a building
getBatimentCoord :: Batiment -> Coord
getBatimentCoord batiment = case batiment of
    Cabane _ coord _ _ -> coord
    Atelier _ coord _ _ -> coord
    Epicerie _ coord _ _ -> coord
    Commissariat _ coord -> coord
    

prop_inv_Batiment :: Etat -> Bool
prop_inv_Batiment etat = prop_carte_coords_valid_Ville etat && prop_carteCoordBat_inv etat

