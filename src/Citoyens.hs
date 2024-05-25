module Citoyens where

import AStarPathfinding
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import GameData

-- getter for coords of a citizen
getCoord :: Citoyen -> Coord
getCoord (Immigrant coord _ _) = coord
getCoord (Habitant coord _ _ _) = coord
getCoord (Emigrant coord _) = coord

-- check if a citizen is at a coordinate
isCitoyenAt :: Coord -> CitId -> Etat -> Bool
isCitoyenAt coord citId (Etat {ville = ville}) =
  let citoyens = getCitoyens ville
   in case Map.lookup citId citoyens of
        Just citoyen -> case citoyen of
          Immigrant coord' _ _ -> coord == coord'
          Habitant coord' _ _ _ -> coord == coord'
          Emigrant coord' _ -> coord == coord'
        Nothing -> False

-- vérifie si les coordonnées et les CitId de l'état sont cohérents
-- ce qui signifie que le citoyen aux coordonnées dans Carte a le même citId
-- que celui dans l'état
prop_carte_coords_valid_Ville :: Etat -> Bool
prop_carte_coords_valid_Ville etat@(Etat {ville = ville, carte = carte}) =
  Map.foldrWithKey step True carte
  where
    step :: Coord -> (BatId, [CitId]) -> Bool -> Bool
    step coord (_, citIds) acc = acc && all (\citId -> isCitoyenAt coord citId etat) citIds

-- regarde si la carte a un citoyen à une coordonnée
hasCitoyenAt :: CitId -> Etat -> Bool
hasCitoyenAt citId etat@(Etat {ville = ville, carte = c}) =
  let citoyens = getCitoyens ville
   in case Map.lookup citId citoyens of
        Just cit -> case cit of
          Immigrant coord _ _ -> lookUpCarteWithCitID coord etat citId
          Habitant coord _ _ _ -> lookUpCarteWithCitID coord etat citId
          Emigrant coord _ -> lookUpCarteWithCitID coord etat citId
        Nothing -> False

-- regarde si le citoyen à une coordonnée dans la carte correspond au citId
lookUpCarteWithCitID :: Coord -> Etat -> CitId -> Bool
lookUpCarteWithCitID coord (Etat {carte = carte}) citId =
  case Map.lookup coord carte of
    Just (_, citIds) -> elem citId citIds
    Nothing -> False

-- récupère un citoyen à une coordonnée (on prend le premier citoyen de la liste)
getCitoyenAt :: Coord -> Etat -> Maybe CitId
getCitoyenAt coord (Etat {carte = carte}) =
  case Map.lookup coord carte of
    Just (_, citIds) -> case citIds of
      [] -> Nothing
      (citId : _) -> Just citId
    Nothing -> Nothing

-- recupère les coordonnées d'un citoyen
getCoordCitoyen :: CitId -> Etat -> Maybe Coord
getCoordCitoyen citId (Etat {ville = ville}) =
  let citoyens = getCitoyens ville
   in case Map.lookup citId citoyens of
        Just cit -> case cit of
          Immigrant coord _ _ -> Just coord
          Habitant coord _ _ _ -> Just coord
          Emigrant coord _ -> Just coord
        Nothing -> Nothing

-- vérifie si les coordonnées des citoyens sont cohérentes avec la carte de l'état
prop_carteCoordCit_inv :: Etat -> Bool
prop_carteCoordCit_inv etat@(Etat {ville = ville, carte = carte}) =
  let citoyens = getCitoyens ville
   in Map.foldrWithKey step True citoyens
  where
    step :: CitId -> Citoyen -> Bool -> Bool
    step citId _ acc =
      acc && case getCoordCitoyen citId etat of
        Just coord ->
          let (_, citIds) = case Map.lookup coord carte of
                Just x -> x
                Nothing -> (BatId 0, [])
           in elem citId citIds && isCitoyenAt coord citId etat -- on vérifie que le citoyen est bien à la coordonnée sur la carte et sur le citoyen lui même
        Nothing -> False

-- smart constructor
-- créé un citoyen si il est valide , il retourne l'état mis à jour et le citId du citoyen
createCitoyen :: Citoyen -> Etat -> (Etat, CitId)
createCitoyen citoyen etat@(Etat {ville = ville}) =
  let citoyens = getCitoyens ville
      citId = CitId (show $ Map.size citoyens + 1)
      updatedCitoyens = Map.insert citId citoyen citoyens
      updatedCarte = updateCarteCitoyen citId (getCoord citoyen) etat
   in (etat {ville = ville {viCit = updatedCitoyens}, carte = updatedCarte}, citId)

-- update la carte avec le nouveau citoyen
updateCarteCitoyen :: CitId -> Coord -> Etat -> Map Coord (BatId, [CitId])
updateCarteCitoyen citId coord etat@(Etat {carte = carte}) =
  case Map.lookup coord carte of
    Just (batId, citIds) -> Map.insert coord (batId, citId : citIds) carte
    Nothing -> Map.insert coord (BatId (-1), [citId]) carte -- BatId -1 is a placeholder

-- update la carte avec le nouveau citoyen
updateCarteCitoyenCoord :: Coord -> Coord -> CitId -> Etat -> Map Coord (BatId, [CitId])
updateCarteCitoyenCoord oldCoord newCoord citId etat = do
  let updatedCarte = removeCarteCitoyenCoord oldCoord citId etat
  updateCarteCitoyen citId newCoord etat {carte = updatedCarte}

-- remove a citizen from the carte at a specific coordinate ( on va dans coord de la carte et on enlève le citId  )
removeCarteCitoyenCoord :: Coord -> CitId -> Etat -> Map Coord (BatId, [CitId])
removeCarteCitoyenCoord coord citId etat = case Map.lookup coord (carte etat) of
  Just (batId, citIds) -> Map.insert coord (batId, filter (/= citId) citIds) (carte etat)
  Nothing -> carte etat

-- remove a citizen from the carte ( on cherche le citId et on l'enlève)
removeCitoyenFromCarte :: CitId -> Etat -> Map Coord (BatId, [CitId])
removeCitoyenFromCarte citId etat@(Etat {carte = carte}) =
  let coord = getCoordCitoyen citId etat
   in case coord of
        Just coord' ->
          let (batId, citIds) = case Map.lookup coord' carte of
                Just x -> x
                Nothing -> (BatId 0, [])
              updatedCitIds = filter (/= citId) citIds
              updatedCarte = Map.insert coord' (batId, updatedCitIds) carte
           in updatedCarte

-- remove a citizen from the ville
removeCitoyenFromVille :: CitId -> Etat -> Etat
removeCitoyenFromVille citId etat@(Etat {ville = ville}) =
  let citoyens = getCitoyens ville
      updatedCitoyens = Map.delete citId citoyens
      updatedCarte = removeCitoyenFromCarte citId etat
   in etat {ville = ville {viCit = updatedCitoyens}, carte = updatedCarte}

-- get the occupation of a citizen
getOccupationCitoyen :: CitId -> Etat -> Maybe Occupation
getOccupationCitoyen citId (Etat {ville = ville}) =
  let citoyens = getCitoyens ville
   in case Map.lookup citId citoyens of
        Just cit -> getOccupation cit
        Nothing -> Nothing
  where
    getOccupation :: Citoyen -> Maybe Occupation
    getOccupation cit1 = case cit1 of
      Immigrant _ _ occ -> Just occ
      Habitant _ _ _ occ -> Just occ
      Emigrant _ occ -> Just occ

-- invariant sur le citoyen (les coordonnées et les citId sont cohérents avec la carte et la ville)
prop_inv_Citoyen :: Etat -> Bool
prop_inv_Citoyen etat = prop_carte_coords_valid_Ville etat && prop_carteCoordCit_inv etat


