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


-- récupère tous les coordonéés de tous les batiments
getBatimentsCoords :: Etat -> [Coord]
getBatimentsCoords (Etat {ville = ville}) =
  let batiments = getBatiments ville
   in Map.foldrWithKey step [] batiments
  where
    step :: BatId -> Batiment -> [Coord] -> [Coord]
    step batId batiment acc = (getBatimentCoord batiment) : acc

-- regarde si un batiment est à une coordonnée donnée 
isBatimentAt :: Coord -> BatId -> Etat -> Bool
isBatimentAt coord batId (Etat {ville = ville}) =
  let batiments = getBatiments ville
   in case Map.lookup batId batiments of
        Just batiment -> (getBatimentCoord batiment) == coord
        Nothing -> False

-- vérifie si les coordonnées et les BatId de l'état sont cohérents
-- ce qui signifie que le bâtiment aux coordonnées dans Carte a le même batId
-- que celui dans l'état
prop_carte_coords_valid_Ville :: Etat -> Bool
prop_carte_coords_valid_Ville etat@(Etat {ville = ville, carte = carte}) = Map.foldrWithKey step True carte
  where
    step :: Coord -> (BatId, [CitId]) -> Bool -> Bool
    step coord (batId, _) acc = acc && isBatimentAt coord batId etat

-- regarde si la carte a un bâtiment à une coordonnée
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

-- regarde si le bâtiment à une coordonnée dans la carte correspond au batId
lookUpCarteWithBatID :: Coord -> Etat -> BatId -> Bool
lookUpCarteWithBatID coord (Etat {carte = carte}) batId = case Map.lookup coord carte of
  Just (batId', _) -> batId == batId'
  Nothing -> False

-- vérifie si tous les bâtiments ont des coordonnées cohérentes avec la carte de l'état
-- ce qui signifie que la carte a le même batId que le bâtiment à la même coordonnée
prop_carteCoordBat_inv :: Etat -> Bool
prop_carteCoordBat_inv etat@(Etat {ville = ville, carte = carte}) =
  let batiments = getBatiments ville
   in Map.foldrWithKey step True batiments
  where
    step :: BatId -> Batiment -> Bool -> Bool
    step batId batiment acc = acc && hasBatimentAt batId etat

-- smaert constructor
-- crée un bâtiment si il est valide , il retourne l'état mis à jour et le batId du bâtiment
createBatiment :: Batiment -> Int -> Etat -> (Etat, BatId)
createBatiment batiment cost etat@(Etat {ville = ville}) =
  case isBatimentValid batiment etat of
    True ->
      let batiments = getBatiments ville
          batId = BatId (Map.size batiments + 1)
          updatedBatiments = Map.insert batId batiment batiments
       in (etat {ville = ville {viBat = updatedBatiments}, coins = (coins etat) - cost}, batId)
    False -> (etat, BatId 0)


-- les bâtiments sont valides s'ils ne sont pas déjà dans la ville
-- et s'ils sont disjoints des bâtiments déjà dans la ville et s'ils sont dans une zone qui est une ZR, ZI ou ZC
isBatimentValid :: Batiment -> Etat -> Bool
isBatimentValid batiment etat@(Etat {ville = ville}) =
  let batiments = getBatiments ville
   in case batiment of
        Cabane forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Atelier forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Epicerie forme coord _ _ -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat
        Commissariat forme coord -> not (elem batiment batiments) && isBatimentDisjoint forme batiments && isBatimentInZone batiment etat

-- regarde si un bâtiment est disjoint des autres bâtiments 
isBatimentDisjoint :: Forme -> Map BatId Batiment -> Bool
isBatimentDisjoint forme batiments =
  let coords = getCoords forme
   in foldr
        ( \batiment acc -> acc && all (\coord -> not (appartient coord (getFormeBatiment batiment))) coords
        )
        True
        (Map.elems batiments)

-- regarde si un bâtiment est dans une zone est correcte : ZR pour les cabanes, ZI pour les ateliers et ZC pour les épiceries etc
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

-- vérifie si un bâtiment est dans une zone
isBatimentInZone' :: Forme -> Batiment -> Bool
isBatimentInZone' forme batiment = all (\coord -> appartient coord forme) (getCoords (getFormeBatiment batiment))

-- get the coordinates of a building
getBatimentCoord :: Batiment -> Coord
getBatimentCoord batiment = case batiment of
  Cabane _ coord _ _ -> coord
  Atelier _ coord _ _ -> coord
  Epicerie _ coord _ _ -> coord
  Commissariat _ coord -> coord

-- invariant pour les bâtiments
prop_inv_Batiment :: Etat -> Bool
prop_inv_Batiment etat = prop_carte_coords_valid_Ville etat && prop_carteCoordBat_inv etat

-- la forme d'un bâtiment
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
      updatedVille = ville' {viBat = updatedBatiments}
   in etat {ville = updatedVille}

-- get coords of bat from batId
getBatimentCoordFromBatId :: BatId -> Etat -> Maybe Coord
getBatimentCoordFromBatId batId etat@(Etat {ville = ville}) =
  let batiments = getBatiments ville
   in case Map.lookup batId batiments of
        Just batiment -> Just (getBatimentCoord batiment)
        Nothing -> Nothing

-- on testait
getBatimentCoordMaybe :: Maybe Batiment -> Maybe Coord
getBatimentCoordMaybe (Just batiment) = Just (getBatimentCoord batiment)
getBatimentCoordMaybe Nothing = Nothing

-- remove a building from the city
removeBatiment :: BatId -> Etat -> Etat
removeBatiment batiment etat@(Etat {ville = ville}) =
  let batiments = getBatiments ville
      updatedBatiments = Map.delete batiment batiments
   in etat {ville = ville {viBat = updatedBatiments}}


-- precondition: il y a des bâtiments dans la ville
prop_pre_removeBatiment :: Etat -> Bool
prop_pre_removeBatiment etat@(Etat {ville = ville}) =
  let batiments = getBatiments ville
   in not (Map.null batiments)

