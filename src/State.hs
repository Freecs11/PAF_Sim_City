module State where


-- state of the game (citizens, buildings, zones, etc.) 
-- we'll serve for simulation too

import GameData
import Batiments 
import Citoyens
import Formes 
import AStarPathfinding

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- init the state
initialiseState :: Int ->  Etat
initialiseState coins  = Etat { 
    ville = Ville { viZones = Map.empty, viBat = Map.empty, viCit = Map.empty },
    coins = coins,
    carte = Map.empty, 
    currentTime = 0, 
    events = Map.empty,
    selection = BuildingType "road"
    }

getSelectedBuilding :: Etat -> Selection
getSelectedBuilding (Etat { selection = selection }) = selection

getTime :: Etat -> Int
getTime (Etat { currentTime = time }) = time


-- menu :: IO (Map.Map String (Int, Int, Int))
-- menu = return $ Map.fromList [("Cabane", (100, 500, 80)), 
--                               ("Epicerie", (200, 500, 95)), 
--                               ("Police", (300, 500, 110)), 
--                               ("Commissariat", (400, 500, 125)),  
--                               ("Railway", (100, 500, 140)),
--                               ("ZoneRoute", (100, 500, 155))
--                               ("ZoneR",  (10, 500, 170)),
--                               ("ZoneI",  (10, 500, 185)),
--                               ("ZoneC",  (10, 500, 200)),
--                               ("ZoneA",  (10, 500, 215)),
--                               ("ZoneE",  (10, 500, 230))
--                               ]
updateSelectedBuilding :: String -> Etat -> Etat
updateSelectedBuilding buildingType state = 
    case buildingType of
        "Cabane" -> state { selection = BuildingType "Cabane" }
        "Atelier" -> state { selection = BuildingType "Atelier" }
        "Epicerie" -> state { selection = BuildingType "Epicerie" }
        "Commissariat" -> state { selection = BuildingType "Commissariat" }
        "ZoneRoute" -> state { selection = ZoneType "ZoneRoute" }
        "ZoneR" -> state { selection = ZoneType "ZoneR" }
        "ZoneI" -> state { selection = ZoneType "ZoneI" }
        "ZoneC" -> state { selection = ZoneType "ZoneC" }
        "ZoneA" -> state { selection = ZoneType "ZoneA" }
        "ZoneE" -> state { selection = ZoneType "ZoneE" }
        _ -> state

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
    -- StartWork citId -> startWork citId state
    -- GoShopping citId -> goShopping citId state
    -- GoHome citId -> goHome citId state
    -- UpdateNeeds citId -> updateNeeds citId state
    -- UpdateHappiness citId -> updateHappiness citId state
    FollowPath (nextCoord:remainingPath) citId ->
        let state' = movingCitizen nextCoord citId state
        in scheduleEvent (currentTime state + 1000) (FollowPath remainingPath citId) state' -- peut etre que le +1000 est trop peu
    FollowPath [] _ -> state  -- Path is complete, do nothing
    TaxRetreival amount -> taxRetreival amount state
    _ -> state


-- fonction pour géré la taxe sur les citoyens
-- elle collecte une taxe sur chaque citoyen de la ville et l'ajoute à la caisse de la ville ( les coins du joueur)
-- et elle planifie un autre événement pour collecter la taxe à nouveau dans 10000000 unités de temps ( TODO : à mettre le temps en variable globale ou autres )
taxRetreival :: Int -> Etat -> Etat
taxRetreival amount state@(Etat { coins = coins , ville = ville }) = 
    let citoyens = Map.elems $ getCitoyens ville
        newState =  foldr (\cit acc -> 
                case cit of
                    Habitant _ _ _ _  -> acc { coins = coins + amount }
                    Immigrant _ _ _ -> acc { coins = coins + amount }
                    _ -> acc
            ) state citoyens
        in
            scheduleEvent (getTime newState + 10000000) (TaxRetreival amount) newState 



-- Event processing functions that concern citizens
-- will calculate the path to the citizen's home and schedule the event to follow it
moveCitizen :: Coord -> CitId -> Etat -> Etat
moveCitizen newCoord citId state@(Etat { ville = v }) =
    let citizen = viCit v Map.! citId -- Map.! retourne une erreur si la clé n'existe pas
        obstacles = Set.fromList $ Map.keys (carte state)
        start = case getCoordCitoyen citId state of
            Just c -> c
            Nothing -> newCoord -- mauvaise gestion de l'erreur ( pour le test TODO /!\ )
        path = aStar obstacles start newCoord
    in case path of
        Just p -> scheduleEvent (currentTime state) (FollowPath p citId) state
        Nothing -> state  -- No path found, do nothing


-- moves a citizen to a new coordinate in the state
movingCitizen :: Coord -> CitId -> Etat -> Etat
movingCitizen newCoord citId state@(Etat { ville = v }) =
    let citizen = viCit v Map.! citId
        newCitizen = updateCitizenCoord newCoord citizen
        newVille = v { viCit = Map.insert citId newCitizen (viCit v) }
    in state { ville = newVille }

updateCitizenCoord :: Coord -> Citoyen -> Citoyen
updateCitizenCoord newCoord (Immigrant _ stats occ) = Immigrant newCoord stats occ
updateCitizenCoord newCoord (Habitant _ stats b occ) = Habitant newCoord stats b occ
updateCitizenCoord newCoord (Emigrant _ occ) = Emigrant newCoord occ
