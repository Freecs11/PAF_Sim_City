module State where


-- state of the game (citizens, buildings, zones, etc.) 
-- we'll serve for simulation too

import GameData
import Batiments 
import Citoyens
import Formes 

import Data.Map (Map)
import qualified Data.Map as Map


-- On va utiliser une fonction pour ajouter un évenement à un temps donné
-- Ex : scheduleEvent (getCurrentTime + 1000) (Move (getHomeCoord citoyen) citoyenId) etat 
-- va ajouter un évenement pour déplacer le citoyen à sa maison dans 1000 unités de temps
-- puisque la boucle de jeu va incrémenter le temps de 1 à chaque tour de boucle , 1 unité de temps = 1 tour de boucle = 1 ms ( à peu près je pense )
-- Function to add an event at a specified time
scheduleEvent :: Int -> Event -> Etat -> Etat
scheduleEvent time event state@(Etat { events = evs }) =
    let updatedEvents = Map.insertWith (++) time [event] evs
    in state { events = updatedEvents }

-- Process all events due at the current time
processEvents :: Int -> Etat -> Etat
processEvents tick state@(Etat { events = evs }) =
    case Map.lookup tick evs of
        Just currentEvents -> foldl (flip processEvent) state currentEvents
        Nothing -> state  -- No events at this tick

-- Process individual events
processEvent :: Event -> Etat -> Etat
processEvent event state = case event of
    Move coord citId -> moveCitizen coord citId state
    StartWork citId -> startWork citId state
    -- GoShopping citId -> goShopping citId state
    -- GoHome citId -> goHome citId state
    -- UpdateNeeds citId -> updateNeeds citId state
    _ -> state


-- startWork :: CitId -> Etat -> Etat
-- startWork citId state@(Etat { ville = ville }) =
--     let citoyens = getCitoyens ville
--     in case Map.lookup citId citoyens of
--         Just citoyen -> case citoyen of
--             Habitant coord _ (batId, _, _) _ -> 
--                 let MaybeCoordBatiment = getBatimentCoord batId state
--                 in case MaybeCoordBatiment of
--                     -- to do 
--             _ -> state
--         Nothing -> state
