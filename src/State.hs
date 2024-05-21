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
import Control.Monad.State (State, get, put, modify)
import Control.Monad (when)


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
updateSelectedBuilding :: String -> State Etat ()
updateSelectedBuilding buildingType = modify $ \state ->
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
scheduleEvent :: Int -> Event -> State Etat ()
scheduleEvent time event = modify $ \state ->
    let updatedEvents = Map.insertWith (++) time [event] (events state)
    in state { events = updatedEvents }

-- Process all events due at the current time
processEvents :: Int -> State Etat ()
processEvents tick = do
    state <- get
    case Map.lookup tick (events state) of
        Just currentEvents -> mapM_ processEvent currentEvents
        Nothing -> return ()

-- Process individual events
processEvent :: Event -> State Etat ()
processEvent event = case event of
    Move coord citId -> moveCitizen coord citId
    -- StartWork citId -> startWork citId
    -- GoShopping citId -> goShopping citId
    -- GoHome citId -> goHome citId
    -- UpdateNeeds citId -> updateNeeds citId
    -- UpdateHappiness citId -> updateHappiness citId
    FollowPath (nextCoord:remainingPath) citId -> do
        movingCitizen nextCoord citId
        state <- get
        scheduleEvent (currentTime state + 1000) (FollowPath remainingPath citId)
    FollowPath [] _ -> return () -- Path is complete, do nothing
    TaxRetreival amount -> taxRetreival amount
    _ -> return ()


-- fonction pour géré la taxe sur les citoyens
-- elle collecte une taxe sur chaque citoyen de la ville et l'ajoute à la caisse de la ville ( les coins du joueur)
-- et elle planifie un autre événement pour collecter la taxe à nouveau dans 10000000 unités de temps ( TODO : à mettre le temps en variable globale ou autres )
taxRetreival :: Int -> State Etat ()
taxRetreival amount = do
    state <- get
    let citoyens = Map.elems $ getCitoyens (ville state)
    let newState = foldr (\cit acc ->
            case cit of
                Habitant _ _ _ _ -> acc { coins = coins acc + amount }
                Immigrant _ _ _ -> acc { coins = coins acc + amount }
                _ -> acc
            ) state citoyens
    put newState
    scheduleEvent (currentTime newState + 10000000) (TaxRetreival amount)



-- Event processing functions that concern citizens
-- will calculate the path to the citizen's home and schedule the event to follow it
moveCitizen :: Coord -> CitId -> State Etat ()
moveCitizen newCoord citId = do
    state <- get
    let citizen = viCit (ville state) Map.! citId -- Map.! returns an error if the key doesn't exist
    let obstacles = Set.fromList $ Map.keys (carte state)
    let start = case getCoordCitoyen citId state of
            Just c -> c
            Nothing -> newCoord -- error handling (for testing TODO /!\)
    let path = aStar obstacles start newCoord
    case path of
        Just p -> scheduleEvent (currentTime state) (FollowPath p citId)
        Nothing -> return () -- No path found, do nothing


-- moves a citizen to a new coordinate in the state
movingCitizen :: Coord -> CitId -> State Etat ()
movingCitizen newCoord citId = modify $ \state ->
    let citizen = viCit (ville state) Map.! citId
        newCitizen = updateCitizenCoord newCoord citizen
        newVille = (ville state) { viCit = Map.insert citId newCitizen (viCit (ville state)) }
    in state { ville = newVille }

-- Helper function to update citizen's coordinates
updateCitizenCoord :: Coord -> Citoyen -> Citoyen
updateCitizenCoord newCoord (Immigrant _ stats occ) = Immigrant newCoord stats occ
updateCitizenCoord newCoord (Habitant _ stats b occ) = Habitant newCoord stats b occ
updateCitizenCoord newCoord (Emigrant _ occ) = Emigrant newCoord occ