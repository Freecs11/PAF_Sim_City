module Batiments where

import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Word (Word8)
import qualified Debug.Trace as T
import Foreign.C.Types (CInt)
import Formes
import GameData
import GameData (Batiment)
import SDL
import SDL (Point (..), Rectangle (..), V4 (..))
import qualified SDL
import qualified SDL.Font as Font
import SDL.Vect (Point (..), V2 (..))
import SDL.Video.Renderer (Rectangle, Renderer, Texture)
import qualified SDL.Video.Renderer as R
import Sprite
import TextureMap

-- TOCHANGE
-- récupère tous les coordonéés de tous les batiments
getBatimentsCoords :: Etat -> [Coord]
getBatimentsCoords (Etat {ville = ville}) =
  let batiments = getBatiments ville
   in Map.foldrWithKey step [] batiments
  where
    step :: BatId -> Batiment -> [Coord] -> [Coord]
    step batId batiment acc = (getBatimentCoord batiment) : acc

-- check if a building is at a coordinate
isBatimentAt :: Coord -> BatId -> Etat -> Bool
isBatimentAt coord batId (Etat {ville = ville}) =
  let batiments = getBatiments ville
   in case Map.lookup batId batiments of
        Just batiment -> (getBatimentCoord batiment) == coord
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
hasBatimentAt batId etat@(Etat {ville = ville, carte = c}) =
  let batiments = getBatiments ville
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
prop_carteCoordBat_inv etat@(Etat {ville = ville, carte = carte}) =
  let batiments = getBatiments ville
   in Map.foldrWithKey step True batiments
  where
    step :: BatId -> Batiment -> Bool -> Bool
    step batId batiment acc = acc && hasBatimentAt batId etat

-- -- smaert constructor
createBatiment :: Batiment -> Int -> Etat  -> Etat
createBatiment batiment cost etat@(Etat {ville = ville})  =
  case isBatimentValid batiment etat of
    True ->
      let batiments = getBatiments ville
          batId = BatId (Map.size batiments + 1)
          updatedBatiments = Map.insert batId batiment batiments
       in etat {ville = ville {viBat = updatedBatiments} , coins = (coins etat) - cost}
    False -> etat


-- buildings are valid if they are not already in the city
-- and if they are disjoint from the buildings already in the city and also are in a zone that is a ZR, ZI or ZC
isBatimentValid :: Batiment -> Etat -> Bool
isBatimentValid batiment etat@(Etat {ville = ville}) =
  let batiments = getBatiments ville
   in case batiment of
        Cabane forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Atelier forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Epicerie forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Commissariat forme coord -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat


-- check if a building is disjoint from the other buildings
isBatimentDisjoint :: Forme -> Map BatId Batiment -> Bool
isBatimentDisjoint forme batiments =
  let coords = getCoords forme
   in foldr
        ( \batiment acc -> acc && all (\coord -> not (appartient coord (getFormeBatiment batiment))) coords
        )
        True
        (Map.elems batiments)


-- check if a building is in a zone that is a ZR, ZI or ZC
isBatimentInZone :: Batiment -> Etat -> Bool
isBatimentInZone batiment etat@(Etat {ville = ville}) =
  let zones = getZones ville
   in foldr
        ( \zone acc -> case (zone, batiment) of
            (ZR forme _, Cabane _ coord _ _) -> acc || isBatimentInZone' forme batiment
            (ZI forme _, Atelier _ coord _ _) -> acc || isBatimentInZone' forme batiment
            (ZC forme _, Epicerie _ coord _ _) -> acc || isBatimentInZone' forme batiment
            (Admin forme _, Commissariat _ coord) -> acc || isBatimentInZone' forme batiment
            _ -> acc
        )
        False
        (Map.elems zones)

isBatimentInZone' :: Forme -> Batiment -> Bool
isBatimentInZone' forme batiment = all (\coord -> appartient coord forme) (getCoords (getFormeBatiment batiment))


-- get the coordinates of a building
getBatimentCoord :: Batiment -> Coord
getBatimentCoord batiment = case batiment of
  Cabane _ coord _ _ -> coord
  Atelier _ coord _ _ -> coord
  Epicerie _ coord _ _ -> coord
  Commissariat _ coord -> coord

prop_inv_Batiment :: Etat -> Bool
prop_inv_Batiment etat = prop_carte_coords_valid_Ville etat && prop_carteCoordBat_inv etat


getFormeBatiment :: Batiment -> Forme
getFormeBatiment (Cabane forme _ _ _) = forme
getFormeBatiment (Atelier forme _ _ _) = forme
getFormeBatiment (Epicerie forme _ _ _) = forme
getFormeBatiment (Commissariat forme _) = forme


-- update a building
updateBatiment :: Batiment -> BatId -> Etat -> Etat
updateBatiment batiment batId etat =
    let ville' = ville etat
        batiments = getBatiments ville'
        updatedBatiments = Map.insert batId batiment batiments
        updatedVille = ville' { viBat = updatedBatiments }
    in etat { ville = updatedVille }