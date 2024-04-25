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
import State

-- TOCHANGE

-- get the map of buildings from a city
getBatiments :: Ville -> Map BatId Batiment
getBatiments Ville { viZones = zones , viBat = batiments} = Map.fromList $ concatMap getBatimentsFromZone $ Map.toList zones
    where
        getBatimentsFromZone :: (ZonId, Zone) -> [(BatId, Batiment)]
        getBatimentsFromZone (zoneId, zone) = case zone of
            ZR _ batimentsId -> foldr (\batId acc -> case Map.lookup batId batiments of
                Just batiment -> (batId, batiment) : acc
                Nothing -> acc) [] batimentsId
            ZI _ batimentsId -> foldr (\batId acc -> case Map.lookup batId batiments of
                Just batiment -> (batId, batiment) : acc
                Nothing -> acc) [] batimentsId
            ZC _ batimentsId -> foldr (\batId acc -> case Map.lookup batId batiments of
                Just batiment -> (batId, batiment) : acc
                Nothing -> acc) [] batimentsId
            Admin _ batId -> case Map.lookup batId batiments of
                Just batiment -> [(batId, batiment)]
                Nothing -> []
            _ -> []



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
createBatiment batId batiment etat@(Etat {ville = ville}) = let batiments = getBatiments ville
    in etat {ville = ville {viBat = Map.insert batId batiment batiments}}

-- remove a building from the city
removeBatiment :: BatId -> Etat -> Etat
removeBatiment batId etat@(Etat {ville = ville}) = let batiments = getBatiments ville
    in etat {ville = ville {viBat = Map.delete batId batiments}}

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



