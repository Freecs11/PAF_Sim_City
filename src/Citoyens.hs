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
isCitoyenAt coord citId (Etat {ville = ville}) = let citoyens = getCitoyens ville
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
prop_carte_coords_valid_Ville etat@(Etat {ville = ville, carte = carte}) = Map.foldrWithKey step True carte
    where
        step :: Coord -> (BatId, CitId) -> Bool -> Bool
        step coord (_, citId) acc = acc && isCitoyenAt coord citId etat

-- -- check if map has a citizen at a coordinate
hasCitoyenAt :: CitId -> Etat -> Bool
hasCitoyenAt citId etat@(Etat {ville = ville , carte =c }) = let citoyens = getCitoyens ville
    in case Map.lookup citId citoyens of
        Just cit -> case cit of
            Immigrant coord _ _ -> lookUpCarteWithCitID coord etat citId
            Habitant coord _ _ _ -> lookUpCarteWithCitID coord etat citId
            Emigrant coord _ -> lookUpCarteWithCitID coord etat citId
        Nothing -> False

-- -- lookup if the citizen at a coordinate in the carte match the citId
lookUpCarteWithCitID :: Coord -> Etat -> CitId -> Bool
lookUpCarteWithCitID coord (Etat {carte = carte}) citId = case Map.lookup coord carte of
    Just (_, citId') -> citId == citId'
    Nothing -> False

-- -- verify if all the citizens have coherent coords with the carte of the state
-- -- meaning that the carte has the same citId as the citizen at the same coord
prop_carteCoordCit_inv :: Etat -> Bool
prop_carteCoordCit_inv etat@(Etat {ville = ville, carte = carte}) = let citoyens = getCitoyens ville
    in Map.foldrWithKey step True citoyens
    where
        step :: CitId -> Citoyen -> Bool -> Bool
        step citId citoyen acc = acc && hasCitoyenAt citId etat



-- add a citizen to the carte 
addCitoyenToCarte :: Coord -> CitId -> Etat -> Etat
addCitoyenToCarte coord citId etat@(Etat {carte = carte}) = etat {carte = Map.insert coord (BatId 0, citId) carte}


updateCitizenNeeds :: CitId -> Etat -> Etat 
updateCitizenNeeds citId state = 
    let citoyens = getCitoyens (ville state)
    in case Map.lookup citId citoyens of
        Just citoyen -> case citoyen of
            Immigrant coord _ _ -> state -- à modifié
            Habitant coord _ _ _ -> state -- à modifié
            Emigrant coord _ -> state -- à modifié
        Nothing -> state -- à modifié


-- function to add an updateNeeds event for each citizen in the state 
updateCitizensNeeds :: Etat -> Etat
updateCitizensNeeds state =     
    let citoyens = getCitoyens (ville state)
    in Map.foldrWithKey step state citoyens
    where
        step :: CitId -> Citoyen -> Etat -> Etat
        step citId _ acc = scheduleEvent (currentTime acc + 10000) (UpdateNeeds citId) acc 
        -- on va mettre à jour les besoins de chaque citoyen dans 10000 unités de temps ( ~ 10 seconde ) , faudra ajuster ce temps pour que ça soit réaliste