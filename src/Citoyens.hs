module Citoyens where

import Data.Map (Map)
import qualified Data.Map as Map


import GameData
import State 


-- -- get the citizens from the ville
getCitoyens :: Ville -> Map CitId Citoyen
getCitoyens Ville { viCit = citoyens } = citoyens



-- -- check if a citizen is at a coordinate
isCitoyenAt :: Coord -> CitId -> Etat -> Bool
isCitoyenAt coord citId (Etat {ville = ville}) = 
    let citoyens = getCitoyens ville
    in case Map.lookup citId citoyens of
        Just citoyen -> case citoyen of
            Immigrant coord' _ _ -> coord == coord'
            Habitant coord' _ _ _ -> coord == coord'
            Emigrant coord' _ -> coord == coord'
        Nothing -> False

-- -- verify if the Coord and CitId of the state are coherent
-- -- meaning that the citizen at the coord in Carte has the same citId
-- -- as the one in the state
prop_carte_coords_valid_Ville :: Etat -> Bool
prop_carte_coords_valid_Ville etat@(Etat {ville = ville, carte = carte}) =
    Map.foldrWithKey step True carte
    where
        step :: Coord -> (BatId, [CitId]) -> Bool -> Bool
        step coord (_, citIds) acc = acc && all (\citId -> isCitoyenAt coord citId etat) citIds

-- -- check if map has a citizen at a coordinate
hasCitoyenAt :: CitId -> Etat -> Bool
hasCitoyenAt citId etat@(Etat {ville = ville , carte =c }) = 
    let citoyens = getCitoyens ville
    in case Map.lookup citId citoyens of
        Just cit -> case cit of 
            Immigrant coord _ _ -> lookUpCarteWithCitID coord etat citId
            Habitant coord _ _ _ -> lookUpCarteWithCitID coord etat citId
            Emigrant coord _ -> lookUpCarteWithCitID coord etat citId
        Nothing -> False

-- -- lookup if the citizen at a coordinate in the carte match the citId
lookUpCarteWithCitID :: Coord -> Etat -> CitId -> Bool
lookUpCarteWithCitID coord (Etat {carte = carte}) citId = 
    case Map.lookup coord carte of
        Just (_, citIds) -> elem citId citIds
        Nothing -> False

-- -- get the citizen at a coordinate
getCitoyenAt :: Coord -> Etat -> Maybe CitId
getCitoyenAt coord (Etat {ville = ville}) = 
    let citoyens = getCitoyens ville
    in case Map.foldrWithKey step Nothing citoyens of
        Just citId -> Just citId
        Nothing -> Nothing
    where
        step :: CitId -> Citoyen -> Maybe CitId -> Maybe CitId
        step citId cit acc = case cit of
            Immigrant coord' _ _ -> if coord == coord' then Just citId else acc
            Habitant coord' _ _ _ -> if coord == coord' then Just citId else acc
            Emigrant coord' _ -> if coord == coord' then Just citId else acc

-- -- get the coordinates of a citizen
getCoordCitoyen :: CitId -> Etat -> Maybe Coord
getCoordCitoyen citId (Etat {ville = ville}) = 
    let citoyens = getCitoyens ville
    in case Map.lookup citId citoyens of
        Just cit -> case cit of
            Immigrant coord _ _ -> Just coord
            Habitant coord _ _ _ -> Just coord
            Emigrant coord _ -> Just coord
        Nothing -> Nothing


prop_carteCoordCit_inv :: Etat -> Bool
prop_carteCoordCit_inv etat@(Etat {ville = ville, carte = carte}) = 
    let citoyens = getCitoyens ville
    in Map.foldrWithKey step True citoyens
    where
        step :: CitId -> Citoyen -> Bool -> Bool
        step citId _ acc = acc && case getCoordCitoyen citId etat of
            Just coord -> isCitoyenAt coord citId etat
            Nothing -> False

-- update the citizen in the carte
updateCit :: CitId -> Map Coord (BatId, [CitId]) -> Etat -> Map Coord (BatId, [CitId])
updateCit citId carte etat = 
    -- get the coordinates of the citizen
    let maybeCoord = getCoordCitoyen citId etat
    in case maybeCoord of
        Just coord -> 
            -- get the current citizen IDs at the coordinate
            let maybeCitIds = Map.lookup coord carte
            in case maybeCitIds of
                Just (batId, citIds) -> 
                    -- check if the citizen ID is already in the list , if not add it
                    if elem citId citIds then carte
                    else Map.insert coord (batId, citId:citIds) carte
                Nothing -> carte
        Nothing -> carte


-- smart constructor
-- create a citizen with a unique id
createCitoyen :: CitId -> Citoyen -> Etat -> Etat
createCitoyen citId citoyen etat@(Etat {ville = ville}) = 
    let citoyens = getCitoyens ville 
    in
    let carte = getCarte etat
    in etat {ville = ville {viCit = Map.insert citId citoyen citoyens       
    } , carte = updateCit citId carte etat }

-- -- remove a citizen from the carte
removeCitFromCarte :: CitId -> Etat -> Map Coord (BatId, [CitId])
removeCitFromCarte citId etat@(Etat {carte = carte}) = 
    let maybeCoord = getCoordCitoyen citId etat
    in case maybeCoord of
        Just coord -> case Map.lookup coord carte of
            Just (batId, citIds) -> Map.insert coord (batId, filter (/= citId) citIds) carte
            Nothing -> carte
        Nothing -> carte

-- remove a citizen from the city
removeCitoyen :: CitId -> Etat -> Etat
removeCitoyen citId etat@(Etat {ville = ville}) = 
    let citoyens = getCitoyens ville
    in etat {ville = ville {viCit = Map.delete citId citoyens}
        , carte = removeCitFromCarte citId etat}

-- get the occupation of a citizen
getOccupationCitoyen :: CitId -> Etat -> Maybe Occupation
getOccupationCitoyen citId (Etat {ville = ville}) = 
    let citoyens = getCitoyens ville
    in case Map.lookup citId citoyens of
        Just cit -> case cit of
            Immigrant _ _ occ -> Just occ
            Habitant _ _ _ occ -> Just occ
            Emigrant _ occ -> Just occ
        Nothing -> Nothing


-- Move a citizen to a new coordinate
moveCitizen :: Coord -> CitId -> Etat -> Etat
moveCitizen newCoord citId etat@(Etat { ville = ville, carte = carte }) =
    let
        -- Fetch the current citizen details
        maybeCitoyen = Map.lookup citId (viCit ville)
        -- Fetch the current coordinates of the citizen
        maybeCitCoord = getCoordCitoyen citId etat
        -- Fetch the current coordinates of the citizen in the carte
        maybeCitCoordCarte = case maybeCitCoord of
            Just coord -> Just coord
            Nothing -> Nothing

        -- Update the citizen's coordinates
        updatedCitoyen = case maybeCitoyen of
            Just citoyen -> updateCitoyenCoord citoyen newCoord
            Nothing -> error "Citizen not found"

        -- Update the citizen in the ville
        updatedCitoyens = Map.insert citId updatedCitoyen (viCit ville)

        -- Update the carte
        updatedCarte = case maybeCitCoordCarte of
            Just coord -> case Map.lookup coord carte of
                Just (batId, citIds) -> Map.insert coord (batId, filter (/= citId) citIds) carte
                Nothing -> carte
            Nothing -> carte
    in etat { ville = ville { viCit = updatedCitoyens }, carte = updatedCarte }

-- Helper function to remove a citizen ID from a list of IDs at a coordinate
removeCitId :: CitId -> [CitId] -> Maybe [CitId]
removeCitId citId citIds = let newCitIds = filter (/= citId) citIds
                           in if null newCitIds then Nothing else Just newCitIds

-- Helper function to update the citizen's coordinate
updateCitoyenCoord :: Citoyen -> Coord -> Citoyen
updateCitoyenCoord (Immigrant _ a b) coord = Immigrant coord a b
updateCitoyenCoord (Habitant _ a b c) coord = Habitant coord a b c
updateCitoyenCoord (Emigrant _ a) coord = Emigrant coord a
