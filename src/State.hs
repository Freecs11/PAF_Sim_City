module State where

-- state of the game (citizens, buildings, zones, etc.)
-- we'll serve for simulation too

import AStarPathfinding
import Batiments
import Batiments (getBatimentCoord)
import Citoyens
import Control.Monad (foldM, when )
import Control.Monad.State (State, get, modify, put)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Formes
import GameData
import GameData (CitId (CitId))
import qualified Control.Monad.State as St
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Control.Monad (forM_)



-- Initialize game state with one building (TESTING)
initialiseStateWithBuilding :: Int -> Etat
initialiseStateWithBuilding coins = Etat
  { ville = 
      Ville
        { viBat = Map.empty,
          viCit = Map.empty,
          viZones = Map.empty
        },
    coins = coins,
    carte = Map.empty,
    currentTime = 0,
    events = Map.empty,
    selection = None,
    world = World { worldOffset = C 0 0 },
    routeDirection = Horizontal,
    selectionStart = Nothing,
    pathCache = Map.empty, -- Initialize the path cache
    pathfindingQueue = [] -- Initialize the pathfinding queue
  }

getSelectedBuilding :: Etat -> Selection
getSelectedBuilding (Etat {selection = selection}) = selection

getTime :: Etat -> Int
getTime (Etat {currentTime = time}) = time

getTimeMonad :: State Etat Int
getTimeMonad = do
  state <- get
  return $ currentTime state

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
    "Cabane" -> state {selection = BuildingType "Cabane"}
    "Atelier" -> state {selection = BuildingType "Atelier"}
    "Epicerie" -> state {selection = BuildingType "Epicerie"}
    "Commissariat" -> state {selection = BuildingType "Commissariat"}
    "ZoneRoute" -> state {selection = ZoneType "ZoneRoute"}
    "ZoneR" -> state {selection = ZoneType "ZoneR"}
    "ZoneI" -> state {selection = ZoneType "ZoneI"}
    "ZoneC" -> state {selection = ZoneType "ZoneC"}
    "ZoneA" -> state {selection = ZoneType "ZoneA"}
    "ZoneE" -> state {selection = ZoneType "ZoneE"}
    _ -> state

-- On va utiliser une fonction pour ajouter un évenement à un temps donné
-- Ex : scheduleEvent (getCurrentTime + 1000) (Move (getHomeCoord citoyen) citoyenId) etat
-- va ajouter un évenement pour déplacer le citoyen à sa maison dans 1000 unités de temps
-- puisque la boucle de jeu va incrémenter le temps de 1 à chaque tour de boucle , 1 unité de temps = 1 tour de boucle = 1 ms ( à peu près je pense )
-- Function to add an event at a specified time
scheduleEvent :: Int -> Event -> State Etat ()
scheduleEvent time event = modify $ \state ->
  let updatedEvents = Map.insertWith (++) time [event] (events state)
   in state {events = updatedEvents}

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
  Move pq -> mapM_ moveCitizen pq
  GoWork citId -> goWork citId
  GoShopping citId -> goShopping citId
  GoHome citId -> goHome citId
  UpdateMoney citId -> updateCitizenMoney citId
  UpdateHunger citId -> updateCitizenHunger citId
  UpdateFatigue citId -> updateCitizenFatigue citId
  UpdateCitizens -> updateCitizens
  FollowPath (nextCoord : remainingPath) citId -> do
    movingCitizen nextCoord citId
    state <- get
    scheduleEvent (currentTime state + 1) (FollowPath remainingPath citId)
  FollowPath [] _ -> return () -- Path is complete, do nothing
  TaxRetreival amount -> taxRetreival amount
  AssignBuildingstoCitizens -> assignBuildingstoCitizens
  PlaceRoute coord -> placeRoute coord
  _ -> return ()


placeRoute :: Coord -> State Etat ()
placeRoute coord = modify $ \state ->
  let newZoneRoute = GameData.Route (GameData.Rectangle coord 120 20)
      newZones = Map.insert (ZonId 1) newZoneRoute (viZones (ville state))
   in state {ville = (ville state) {viZones = newZones}}

-- fonction pour géré la taxe sur les citoyens
-- elle collecte une taxe sur chaque citoyen de la ville et l'ajoute à la caisse de la ville ( les coins du joueur)
-- et elle planifie un autre événement pour collecter la taxe à nouveau dans 10000000 unités de temps ( TODO : à mettre le temps en variable globale ou autres )
taxRetreival :: Int -> State Etat ()
taxRetreival amount = do
  state <- get
  let citoyens = Map.elems $ getCitoyens (ville state)
  let newState =
        foldr
          ( \cit acc ->
              case cit of
                Habitant _ _ _ _ -> acc {coins = coins acc + amount}
                Immigrant _ _ _ -> acc {coins = coins acc + amount}
                _ -> acc
          )
          state
          citoyens
  put newState
  scheduleEvent (currentTime newState + 10000) (TaxRetreival amount)

-- moveCitizen :: Coord -> CitId -> State Etat ()
-- moveCitizen newCoord citId = do
--   state <- St.get
--   let citizen = viCit (ville state) Map.! citId -- Map.! returns an error if the key doesn't exist
--   let start = case getCoordCitoyen citId state of
--         Just c -> c
--         Nothing -> newCoord
--   let path = aStar start newCoord state
--   case path of
--     Just (_, p) -> 
--       let testPath = [(C 100 100), (C 100 200), (C 100 300), (C 100 400), (C 100 500), (C 100 600), (C 100 700), (C 100 800), (C 100 900), (C 100 1000)]
--       -- trace to display the path
--       in
--       scheduleEvent ((currentTime state) + 300) (FollowPath p citId)
--     Nothing -> return () -- No path found, do nothing

-- getClosestRouteToCitoyen :: Maybe Coord -> Etat -> Maybe Coord
-- getClosestRouteToCitoyen Nothing _ = Nothing
-- getClosestRouteToCitoyen (Just coord) state =
--   let routes = Map.elems $ Map.filter isRoute (viZones $ ville state)
--       closestRoute = foldr (\route acc -> 
--                               let routeCoord = getZoneCoord route
--                                   accCoord = fromMaybe coord acc
--                               in if mdistance coord routeCoord < mdistance coord accCoord 
--                                  then Just routeCoord 
--                                  else acc) 
--                            Nothing 
--                            routes
--   in closestRoute



moveCitizen :: PathfindingRequest -> State Etat ()
moveCitizen (PathfindingRequest citId newCoord) = do
  state <- St.get
  let citizen = viCit (ville state) Map.! citId
  let citCoord = fromMaybe (newCoord) (getCoordCitoyen citId state)
  let start = fromMaybe citCoord  (getCoordCitoyen citId state) 
  let nd = fromMaybe newCoord  (Just newCoord) 
  path <- aStar start nd
  case path of
    Just (_, p) -> do
      -- Print the path for debugging
      scheduleEvent ((currentTime state) + 300) (FollowPath p citId)
    Nothing -> return () -- No path found, do nothing

-- Add a function to enqueue pathfinding requests
enqueuePathfindingRequest :: CitId -> Coord -> State Etat ()
enqueuePathfindingRequest citId newCoord = do
  state <- St.get
  let queue = pathfindingQueue state
  let newQueue = queue ++ [PathfindingRequest citId newCoord]
  put state { pathfindingQueue = newQueue }

-- Process a fixed number of pathfinding requests per tick
processPathfindingQueue :: Int -> State Etat ()
processPathfindingQueue batchSize = do
  state <- St.get
  let queue = pathfindingQueue state
  let (toProcess, remainingQueue) = splitAt batchSize queue
  mapM_ moveCitizen toProcess
  put state { pathfindingQueue = remainingQueue }



getClosestRouteToCitoyen :: Maybe Coord -> Etat -> Maybe Coord
getClosestRouteToCitoyen Nothing _ = Nothing
getClosestRouteToCitoyen (Just coord) state =
  let routes = Map.elems $ Map.filter isRoute (viZones $ ville state)
      closestRoute = foldr (\route acc ->
                              let routeCoord = getZoneCoord route
                                  accCoord = fromMaybe coord acc
                              in if mdistance coord routeCoord < mdistance coord accCoord
                                 then Just routeCoord
                                 else acc)
                           Nothing
                           routes
  in closestRoute

isRoute :: Zone -> Bool
isRoute (Route _) = True
isRoute _ = False

getZoneCoord :: Zone -> Coord
getZoneCoord (Route forme) = getFormeCoord forme
getZoneCoord _ = error "Not a route"

mdistance :: Coord -> Coord -> Int
mdistance (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)



goHome :: CitId -> State Etat ()
goHome citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) _ -> do
      let homeCoord = getBatimentCoordFromBatId home state
      case homeCoord of
        Just c -> do
          let newCitizen = Habitant c (money, fatigue, hunger) (home, work, shop) (SeDeplacer c)
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (Move [PathfindingRequest citId c])
        Nothing -> put state

goShopping :: CitId -> State Etat ()
goShopping citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) _ -> do
      shopID <- case shop of
        Just s -> return s
        Nothing -> return (BatId 0)
      let shopCoord = getBatimentCoordFromBatId shopID state
      case shopCoord of
        Just c -> do
          let newCitizen = Habitant c (money, fatigue, hunger) (home, work, shop) (SeDeplacer c)
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (Move [PathfindingRequest citId c])
        Nothing -> put state

goWork :: CitId -> State Etat ()
goWork citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) _ -> do
      workID <- case work of
        Just w -> return w
        Nothing -> return (BatId 0)
      let workCoord = getBatimentCoordFromBatId workID state
      case workCoord of
        Just c -> do
          let newCitizen = Habitant coord (money, fatigue, hunger) (home, work, shop) (SeDeplacer c)
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (Move [PathfindingRequest citId c])
        Nothing -> put state

updateCitizenMoney :: CitId -> State Etat ()
updateCitizenMoney citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) statut ->
      if money < 50
        then do
          let newCitizen = Habitant coord (money + 100, fatigue, hunger) (home, work, shop) statut
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (GoWork citId)
          scheduleEvent ((currentTime state) + 1000) (UpdateMoney citId)
        else do
          put state

updateCitizenHunger :: CitId -> State Etat ()
updateCitizenHunger citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) statut ->
      if hunger < 50
        then do
          let newCitizen = Habitant coord (money - 20, fatigue - 20, hunger + 50) (home, work, shop) statut
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (GoShopping citId)
          scheduleEvent ((currentTime state) + 1000) (UpdateHunger citId)
        else do
          put state

updateCitizenFatigue :: CitId -> State Etat ()
updateCitizenFatigue citId = do
  state <- get
  let citizen = viCit (ville state) Map.! citId
  case citizen of
    Habitant coord (money, fatigue, hunger) (home, work, shop) statut ->
      if fatigue <= 0
        then do
          let newCitizen = Habitant coord (money, fatigue + 100, hunger) (home, work, shop) statut
          let newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
          put state {ville = newVille}
          scheduleEvent ((currentTime state) + 100) (GoHome citId)
          scheduleEvent ((currentTime state) + 1000) (UpdateFatigue citId)
        else do
          put state

updateCitizens :: State Etat ()
updateCitizens = do
  state <- get
  let citoyens = Map.keys (viCit (ville state))
  newState <- foldM updateCitizen state citoyens
  put newState
  where
    updateCitizen :: Etat -> CitId -> State Etat Etat
    updateCitizen st citID = do
      let (Habitant coord (money, fatigue, hunger) (home, work, shop) _) = viCit (ville st) Map.! citID
      let homeBatCoord = getBatimentCoord (viBat (ville st) Map.! home)
      workID <- case work of
        Just w -> return w
        Nothing -> return (BatId 0)
      let workBatCoord = getBatimentCoord (viBat (ville st) Map.! workID)
      shopID <- case shop of
        Just s -> return s
        Nothing -> return (BatId 0)
      let shopBatCoord = getBatimentCoord (viBat (ville st) Map.! shopID)
      let time = currentTime st

      let newCitizen = case coord of
            _
              | coord == homeBatCoord ->
                  Habitant coord (money, fatigue, hunger) (home, work, shop) Dormir
              | coord == workBatCoord ->
                  Habitant coord (money, fatigue, hunger) (home, work, shop) Travailler
              | coord == shopBatCoord ->
                  Habitant coord (money, fatigue, hunger) (home, work, shop) FaireCourses
              | otherwise ->
                  Habitant coord (money, fatigue, hunger) (home, work, shop) (SeDeplacer coord)

      let newVille = (ville st) {viCit = Map.insert citID newCitizen (viCit (ville st))}
      let newState = st {ville = newVille}

      case coord of
        _
          | coord == homeBatCoord -> scheduleEvent (time + 800) (GoWork citID)
          | coord == workBatCoord -> scheduleEvent (time + 800) (GoShopping citID)
          | coord == shopBatCoord -> scheduleEvent (time + 200) (GoHome citID)
          | otherwise -> return ()

      return newState

-- moves a citizen to a new coordinate in the state
movingCitizen :: Coord -> CitId -> State Etat ()
movingCitizen newCoord citId = modify $ \state ->
  let citizen = viCit (ville state) Map.! citId
      newCitizen = updateCitizenCoord newCoord citizen
      newVille = (ville state) {viCit = Map.insert citId newCitizen (viCit (ville state))}
   in state {ville = newVille}

-- Helper function to update citizen's coordinates
updateCitizenCoord :: Coord -> Citoyen -> Citoyen
updateCitizenCoord newCoord (Immigrant _ stats occ) = Immigrant newCoord stats occ
updateCitizenCoord newCoord (Habitant _ stats b occ) = Habitant newCoord stats b occ
updateCitizenCoord newCoord (Emigrant _ occ) = Emigrant newCoord occ

assignBuildingstoCitizens :: State Etat ()
assignBuildingstoCitizens = do
  state <- get
  let citoyens = getCitoyens (ville state) -- Map CitId Citoyen
  let batiments = getBatiments (ville state) -- Map BatId Batiment

  -- Get work and shopping buildings with available slots
  let workBuildings = Map.filter (\bat -> case bat of
                        Atelier _ _ maxCit _ -> maxCit > 0
                        _ -> False) batiments
  let shoppingBuildings = Map.filter (\bat -> case bat of
                           Epicerie _ _ maxCit _ -> maxCit > 0
                           _ -> False) batiments

  let habitants =
        Map.filter
          ( \cit -> case cit of
              Habitant _ _ _ _ -> True
              _ -> False
          )
          citoyens
  let habitantIds = Map.keys habitants

  -- Assign work to Habitants (if they don't have one already)
  let habitantWorkAssignments = assignCitizensToBuildings habitantIds workBuildings

  -- Update citizens with work assignments
  let newCitoyensWithWork =
        foldr
          ( \(citId, batId) acc ->
              case Map.lookup citId acc of
                Just (Habitant coord stats (home, _, shop) occ) -> Map.insert citId (Habitant coord stats (home, Just batId, shop) occ) acc
                _ -> acc
          )
          citoyens
          habitantWorkAssignments

  -- Re-filter citizens to update shopping assignments
  let habitants' =
        Map.filter
          ( \cit -> case cit of
              Habitant _ _ _ _ -> True
              _ -> False
          )
          newCitoyensWithWork
  let habitantIds' = Map.keys habitants'

  -- Assign shopping destinations to Habitants (if they don't have one already)
  let habitantShoppingAssignments = assignCitizensToBuildings habitantIds' shoppingBuildings

  -- Update citizens with shopping assignments
  let newCitoyensWithShopping =
        foldr
          ( \(citId, batId) acc ->
              case Map.lookup citId acc of
                Just (Habitant coord stats (home, work, _ ) occ) -> Map.insert citId (Habitant coord stats (home, work, Just batId) occ) acc
                _ -> acc
          )
          newCitoyensWithWork
          habitantShoppingAssignments

  put $ state {ville = (ville state) {viCit = newCitoyensWithShopping}}

-- Assign citizens to buildings based on availability
assignCitizensToBuildings :: [CitId] -> Map BatId Batiment -> [(CitId, BatId)]
assignCitizensToBuildings [] _ = []
assignCitizensToBuildings _ buildings | Map.null buildings = []
assignCitizensToBuildings (citId:citIds) buildings =
  case findAvailableBuilding buildings of
    Just (batId, updatedBuildings) ->
      (citId, batId) : assignCitizensToBuildings citIds updatedBuildings
    Nothing -> []

-- Find an available building and update its capacity
findAvailableBuilding :: Map BatId Batiment -> Maybe (BatId, Map BatId Batiment)
findAvailableBuilding buildings =
  case Map.toList $ Map.filter hasCapacity buildings of
    ((batId, batiment):_) -> Just (batId, Map.adjust decreaseCapacity batId buildings)
    _ -> Nothing

-- Check if a building has capacity for more citizens
hasCapacity :: Batiment -> Bool
hasCapacity (Atelier _ _ maxCit _) = maxCit > 0
hasCapacity (Epicerie _ _ maxCit _) = maxCit > 0
hasCapacity _ = False

-- Decrease the capacity of a building
decreaseCapacity :: Batiment -> Batiment
decreaseCapacity (Atelier d coord maxCit ctz) = Atelier d coord (maxCit - 1) ctz
decreaseCapacity (Epicerie d coord maxCit ctz) = Epicerie d coord (maxCit - 1) ctz
decreaseCapacity bat = bat

