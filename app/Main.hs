{-# LANGUAGE OverloadedStrings #-}

module Main where

import Batiments
import Citoyens
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, unless, when)
import Control.Monad.State (State, evalState, execState, get, modify, put, runState)
import qualified Control.Monad.State as St
import Data.List (foldl', minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word8)
import qualified Debug.Trace as T
import Foreign.C.Types (CInt (..))
import Formes
import GameData (BatId (..), Batiment (..), CitId (..), Citoyen (..), Coord (..), Etat (..), Forme (..), RouteDirection (..), Selection (..), Ville (..), World (..), ZonId (..), Zone (..), getCarte)
import qualified GameData as GameData
import Keyboard (Keyboard (..))
import qualified Keyboard as K
import Linear (V2 (..), V4 (..))
import Mouse (MouseState (..), getMousePosition, mouseButtonPressed)
import qualified Mouse as Ms
import SDL
import qualified SDL.Font as Font
import SDL.Time (delay, time)
import SDL.Vect (Point (P))
import Sprite (Sprite)
import qualified Sprite as S
import SpriteMap (SpriteId (..), SpriteMap)
import qualified SpriteMap as SM
import qualified State as State
import TextureMap (TextureId (..), TextureMap)
import qualified TextureMap as TM
import Zone
import System.Random (randomRs, mkStdGen)

windowsWidth :: CInt
windowsWidth = 1280

windowsHeight :: CInt
windowsHeight = 680

tailleBloc :: CInt
tailleBloc = 50

tailleBlocI :: Int
tailleBlocI = 10

routeFormeHoriz :: Coord -> Int -> Forme
routeFormeHoriz coord l = GameData.Rectangle coord l 20

routeFormeVert :: Coord -> Int -> Forme
routeFormeVert coord l = GameData.Rectangle coord 20 l

rectangularZone :: Coord -> Int -> Int -> Forme
rectangularZone coord w h = GameData.Rectangle coord w h

-- TO BE MODIFIED --------------------------------------------------------------

loadBackground :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "grass") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "grass") (S.mkArea 0 0 8000 8000)
  let smap' = SM.addSprite (SpriteId "grass") sprite smap
  return (tmap', smap')

-- | Load the perso sprite
loadPerso :: Renderer -> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 20 20)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

-- | Load the building sprites
loadBuildings :: Renderer -> [(TextureId, FilePath)] -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBuildings rdr [] tmap smap = return (tmap, smap)
loadBuildings rdr ((tid, path) : xs) tmap smap = do
  tmap' <- TM.loadTexture rdr path tid tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage tid (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId $ show tid) sprite smap
  loadBuildings rdr xs tmap' smap'

loadBackgroundColor :: Renderer -> V4 Word8 -> IO ()
loadBackgroundColor renderer color = do
  rendererDrawColor renderer $= color
  clear renderer
  present renderer

-- contient uniquement les batiments
menu :: IO (Map.Map String (Int, Int, Int))
menu =
  return $
    Map.fromList
      [ ("Cabane", (80, 1100, 80)),
        ("Epicerie", (95, 1100, 95)),
        ("Atelier", (110, 1100, 110)),
        ("Commissariat", (125, 1100, 125))
      ]

renderMenuItems :: Renderer -> Font.Font -> Map.Map String (Int, Int, Int) -> IO ()
renderMenuItems renderer font menuItems = do
  -- on render chaque item du menu
  Map.traverseWithKey
    ( \name (cost, x, y) ->
        S.displayText renderer font (pack $ name ++ " : " ++ show cost) (V2 (fromIntegral x) (fromIntegral y)) (V4 255 255 255 255)
    )
    menuItems
  return ()

zonesMenu :: IO (Map.Map String (Int, Int))
zonesMenu =
  return $
    Map.fromList
      [ ("ZoneRoute", (1100, 170)),
        ("ZoneR", (1100, 195)),
        ("ZoneI", (1100, 220)),
        ("ZoneC", (1100, 245)),
        ("ZoneA", (1100, 270)),
        ("ZoneE", (1100, 295))
      ]

renderZonesMenuItems :: Renderer -> Font.Font -> Map.Map String (Int, Int) -> IO ()
renderZonesMenuItems renderer font menuItems = do
  -- on render chaque item du menu
  Map.traverseWithKey
    ( \name (x, y) ->
        S.displayText renderer font (pack $ name) (V2 (fromIntegral x) (fromIntegral y)) (V4 255 255 255 255)
    )
    menuItems
  return ()

-- check if the mouse is over a menu item
isMouseOverMenuItem :: (Int, Int) -> (Int, Int, Int) -> Bool
isMouseOverMenuItem (x, y) (x', y', w) = x > x' && x < x' + w && y > y' && y < y' + 20

-- | Get the texture id of a sprite
getTextureId :: Sprite -> TextureId
getTextureId sprite = case S.currentImage sprite of
  S.Image tid _ -> tid

-- Adjust for world offset
applyOffset :: Coord -> Coord -> Coord
applyOffset (C x y) (C dx dy) = C (x + dx) (y + dy)

renderBackgroundMap :: Renderer -> TextureMap -> SpriteMap -> Coord -> IO ()
renderBackgroundMap renderer tmap smap offset = do
  let sprite = SM.fetchSprite (SpriteId "grass") smap
  let (x, y) = getXY offset
  S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral (x - 4000)) (fromIntegral (y - 4000)))

renderBuildings :: Renderer -> TextureMap -> SpriteMap -> Ville -> Coord -> IO ()
renderBuildings renderer tmap smap ville offset = do
  let buildings = Map.elems $ viBat ville
  mapM_ (renderBuilding renderer tmap smap offset) buildings

renderBuilding :: Renderer -> TextureMap -> SpriteMap -> Coord -> Batiment -> IO ()
renderBuilding renderer tmap smap offset building = do
  let (coord, spriteId) = case building of
        Cabane _ coord _ _ -> (coord, SpriteId "Cabane")
        Atelier _ coord _ _ -> (coord, SpriteId "Atelier")
        Epicerie _ coord _ _ -> (coord, SpriteId "Epicerie")
        Commissariat _ coord -> (coord, SpriteId "Commissariat")
  let sprite = SM.fetchSprite spriteId smap
  let (x, y) = getXY $ applyOffset coord offset
  S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral x) (fromIntegral y))

loadElement :: Renderer -> FilePath -> String -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadElement rdr path id tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId id) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId id) (S.mkArea 0 0 tailleBloc tailleBloc)
  let smap' = SM.addSprite (SpriteId id) sprite smap
  return (tmap', smap')

-- Run the game loop, processing IO and state separately
main :: IO ()
main = do
  initializeAll
  Font.initialize
  window <- createWindow "Sim City" $ defaultWindow {windowInitialSize = V2 windowsWidth windowsHeight}
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/micropolisMap.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap'', smap'') <- loadPerso renderer "assets/perso.bmp" tmap smap

  -- background color is black for now ( will load dirt texture later)
  -- loadBackgroundColor renderer (V4 0 0 0 255)

  -- liste d'éléments (Texture ID, FilePath) à charger
  let buildings =
        [ ("Cabane", "assets/residuel.bmp"),
          ("Atelier", "assets/atelier.bmp"),
          ("Epicerie", "assets/epicerie.bmp"),
          ("Commissariat", "assets/police.bmp")
        ]
  (tmap', smap') <- foldM (\(t, s) (id, path) -> loadElement renderer path id t s) (tmap'', smap'') buildings

  let startCoins = 3000
  let taxesCitizens = 100
  -- initialisation de l'état du jeu , batiment selectionné par défaut est les routes
  let gameState0 = State.initialiseStateWithBuilding startCoins

  let retrEvent = GameData.TaxRetreival taxesCitizens
  -- let placeRout = GameData.PlaceRoute (C 107 255)
  -- let cit1GowORK = GameData.GoWork (CitId (show 1))
  -- on schedule un évenement pour prélever des taxes sur les citoyens tous les 1000000 unités de temps
  let gameState =
        execState
          ( do
              State.scheduleEvent 1000 retrEvent
              -- State.scheduleEvent 2000 placeRout
              -- State.scheduleEvent 3000 cit1GowORK
          )
          gameState0

  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  let mouse = Ms.createMouseState

  -- Load menu items
  menuItems <- menu

  -- Load zones menu items
  zonesMenuItems <- zonesMenu

  -- faut télécharger SDL2_ttf pour que ça marche
  font <- Font.load "assets/Nexa-Heavy.ttf" 15

  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd gameState mouse font menuItems zonesMenuItems

  putStrLn "Exiting..."

-- Game loop now processes IO and state updates separately
-- Game loop now processes IO and state updates separately
gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Etat -> MouseState -> Font.Font -> Map.Map String (Int, Int, Int) -> Map.Map String (Int, Int) -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState mouse font menuItems zonesMenuItems = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mouse' = Ms.handleMouseEvents events mouse -- Update mouse state based on events
  let gameStateW = moveWorld kbd' gameState

  clear renderer

  let selectedBuilding = case State.getSelectedBuilding gameState of
        BuildingType building -> building
        ZoneType zone -> zone
        None -> "None"
  --- display background
  -- S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "grass") smap)
  renderBackgroundMap renderer tmap smap (worldOffset $ world gameStateW)
  -- Display the selected building on the screen top left
  -- S.displayText renderer font ("Selected building: " <> pack selectedBuilding) (V2 3 5) (V4 255 255 255 255)

  -- display player coins on the screen top left
  S.displayText renderer font ("Coins: " <> pack (show $ coins gameStateW)) (V2 3 5) (V4 255 255 255 255)

  let sizeCitizens = Map.size $ viCit $ ville gameStateW

  -- display number of citizens on the screen top left
  S.displayText renderer font ("Citizens: " <> pack (show sizeCitizens)) (V2 3 35) (V4 255 255 255 255)

  -- display current time on the screen bottom left
  S.displayText renderer font ("Time: " <> pack (show $ currentTime gameStateW)) (V2 3 50) (V4 255 255 255 255)

  -- print first citizen
  printFirstCitizen gameStateW

  -- let isKeyPressed = K.keypressed KeycodeZ kbd'
  -- when isKeyPressed $ putStrLn "Z key pressed"
  -- Toggle route direction when key R is pressed
  let gameStateW' = if K.keypressed KeycodeR kbd' then toggleRouteDirection gameStateW else gameStateW

  if selectedBuilding == "ZoneRoute"
    then do
      let routeDir = routeDirection gameStateW'
      S.displayText renderer font ("Route direction: " <> pack (show routeDir)) (V2 3 20) (V4 255 255 255 255)
    else return ()

  -- Render buildings with world offset
  renderBuildings renderer tmap smap (ville gameStateW) (worldOffset $ world gameStateW)
  -- Render zones with world offset
  renderZones renderer (ville gameStateW) (worldOffset $ world gameStateW)
  -- Render citizens with world offset
  renderCitizens renderer tmap smap (ville gameStateW) (worldOffset $ world gameStateW)
  -- Display menu
  renderMenuItems renderer font menuItems
  -- Display zones menu
  renderZonesMenuItems renderer font zonesMenuItems
  -- update the screen background color to black
  -- SDL.rendererDrawColor renderer $= V4 0 0 0 255

  present renderer

  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  let deltaTime = endTime - startTime
  -- Update menu items based on mouse events
  (updatedMenuItems, currentSelectedBuilding) <- updateMenuItems menuItems zonesMenuItems selectedBuilding mouse' events
  -- print all existing zones in the game state
  -- putStrLn $ show $ viZones $ ville gameState
  gameStateStr <- createStructureIfNeeded currentSelectedBuilding mouse' events gameStateW' updatedMenuItems
  -- Check if a building needs to be created
  gameState'' <- createBuildingIfNeeded currentSelectedBuilding mouse' events gameStateStr updatedMenuItems

  -- Spawn citizens
  gameStateCitizens <- spawnCitizens gameState''

  -- let updateEvent = GameData.UpdateCitizens

  -- scheduleEvent
  -- let gameStateWithEvent = execState (State.scheduleEvent (State.getTime gameStateW + 10) updateEvent) gameStateCitizens

   -- Process a fixed number of pathfinding requests per tick
  let batchSize = 10 -- Adjust the batch size as needed
  let gameStateProcessed = execState (State.processPathfindingQueue batchSize) gameStateCitizens

  -- Update game state using State monad
  let gameState' =
        execState
          ( do
              State.processEvents (State.getTime gameStateProcessed)
              State.updateSelectedBuilding currentSelectedBuilding
              State.scheduleEvent (State.getTime gameStateProcessed + 10) GameData.AssignBuildingstoCitizens
          )
          gameStateProcessed
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' {currentTime = (currentTime gameState') + 1} mouse' font updatedMenuItems zonesMenuItems)

toggleRouteDirection :: Etat -> Etat
toggleRouteDirection etat =
  let direction = routeDirection etat
   in etat {routeDirection = if direction == Horizontal then Vertical else Horizontal}

createBuildingIfNeeded :: String -> MouseState -> [Event] -> Etat -> Map.Map String (Int, Int, Int) -> IO Etat
createBuildingIfNeeded selectedBuilding mouseEvent events gameState menuItems =
  if Ms.mouseButtonPressed mouseEvent events
    then do
      let (x, y) = Ms.getMousePosition mouseEvent
      let offset = worldOffset $ world gameState
      let gameCoord = applyOffset (C (fromIntegral x) (fromIntegral y)) (negateCoord offset)
      let isOutsideMenu = not $ any (\(_, (_, mx, my)) -> isMouseOverMenuItem (x, y) (mx, my, 100)) (Map.toList menuItems)
      if isOutsideMenu
        then return $ execState (createBuilding selectedBuilding gameCoord) gameState
        else return gameState
    else return gameState

negateCoord :: Coord -> Coord
negateCoord (C x y) = C (-x) (-y)

createBuilding :: String -> Coord -> State Etat ()
createBuilding "Cabane" coord = do
  etat <- St.get
  let batiment = Cabane (GameData.Rectangle coord 50 50) coord 10 []
  -- update menu items ( number of coins it costs to build the building)
  if coins etat >= 80
    then do
      let (newStat, _) = createBatiment batiment 80 etat
      put $ newStat
    else return ()
createBuilding "Atelier" coord = do
  etat <- St.get
  let batiment = Atelier (GameData.Rectangle coord 50 50) coord 20 []
  if coins etat >= 95
    then do
      let (newStat, _) = createBatiment batiment 95 etat
      put $ newStat
    else return ()
createBuilding "Epicerie" coord = do
  etat <- St.get
  let batiment = Epicerie (GameData.Rectangle coord 50 50) coord 20 []
  if coins etat >= 110
    then do
      let (newStat, _) = createBatiment batiment 110 etat
      put $ newStat
    else return ()
createBuilding "Commissariat" coord = do
  etat <- St.get
  let batiment = Commissariat (GameData.Rectangle coord 50 50) coord
  if coins etat >= 125
    then do
      let (newStat, _) = createBatiment batiment 125 etat
      put $ newStat
    else return ()
createBuilding _ _ = return ()

updateMenuItems :: Map.Map String (Int, Int, Int) -> Map.Map String (Int, Int) -> String -> MouseState -> [Event] -> IO (Map.Map String (Int, Int, Int), String)
updateMenuItems menuItems zonesMenuItems selectedBuilding mouseEvent events =
  if Ms.mouseButtonPressed mouseEvent events
    then do
      let (x, y) = Ms.getMousePosition mouseEvent
      let clickedItem = Map.filterWithKey (\_ (cost, x', y') -> x > fromIntegral x' && x < fromIntegral (x' + 100) && y > fromIntegral y' && y < fromIntegral (y' + 20)) menuItems
      let clickedZoneItem = Map.filterWithKey (\_ (x', y') -> x > fromIntegral x' && x < fromIntegral (x' + 100) && y > fromIntegral y' && y < fromIntegral (y' + 20)) zonesMenuItems

      if not (Map.null clickedItem)
        then do
          let selectedItem = head $ Map.toList clickedItem
          let (itemName, (cost, x', y')) = selectedItem
          putStrLn $ "Clicked on: " <> itemName
          return (menuItems, itemName)
        else
          if not (Map.null clickedZoneItem)
            then do
              let selectedZoneItem = head $ Map.toList clickedZoneItem
              let (zoneName, (x', y')) = selectedZoneItem
              putStrLn $ "Clicked on zone: " <> zoneName -- Display the name of the clicked zone
              return (menuItems, zoneName)
            else return (menuItems, selectedBuilding) -- No item was clicked
    else return (menuItems, selectedBuilding) -- No mouse button was pressed

-- Move the whole world when arrow keys are pressed
moveWorld :: Keyboard -> Etat -> Etat
moveWorld kbd etat =
  let dx = if K.keypressed KeycodeD kbd then 10 else if K.keypressed KeycodeQ kbd then -10 else 0
      dy = if K.keypressed KeycodeS kbd then 10 else if K.keypressed KeycodeZ kbd then -10 else 0
      offset = worldOffset (world etat)
      newOffset = applyOffset offset (C dx dy)
   in etat {world = (world etat) {worldOffset = newOffset}}

-- | Check if a building is outside the menu
isOutsideMenu :: Coord -> Bool
isOutsideMenu (C x y) = x < 1100

renderZones :: Renderer -> Ville -> Coord -> IO ()
renderZones renderer ville offset = do
  let zones = Map.elems $ viZones ville
  mapM_ (renderZone renderer offset) zones

renderZone :: Renderer -> Coord -> Zone -> IO ()
renderZone renderer offset (Route forme) = do
  -- for route , draw blue rectangle
  let (x, y) = getXY $ applyOffset (zoneCoord (Route forme)) offset
  let (w, h) = getWH (zoneForme (Route forme))
  SDL.rendererDrawColor renderer $= V4 0 0 255 255
  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))
renderZone renderer offset (Eau forme) = do
  -- for water , draw cyan rectangle
  let (x, y) = getXY $ applyOffset (zoneCoord (Eau forme)) offset
  let (w, h) = getWH (zoneForme (Eau forme))
  SDL.rendererDrawColor renderer $= V4 0 255 255 255
  SDL.fillRect renderer (Just (SDL.Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))
renderZone renderer offset zone = do
  let (x, y) = getXY $ applyOffset (zoneCoord zone) offset
  let (w, h) = getWH (zoneForme zone)
  -- Draw the top border
  SDL.rendererDrawColor renderer $= zoneBorderColor zone
  SDL.drawLine renderer (P (V2 (fromIntegral x) (fromIntegral y))) (P (V2 (fromIntegral (x + w)) (fromIntegral y)))

  -- Draw the bottom border
  SDL.drawLine renderer (P (V2 (fromIntegral x) (fromIntegral (y + h)))) (P (V2 (fromIntegral (x + w)) (fromIntegral (y + h))))

  -- Draw the left border
  SDL.drawLine renderer (P (V2 (fromIntegral x) (fromIntegral y))) (P (V2 (fromIntegral x) (fromIntegral (y + h))))

  -- Draw the right border
  SDL.drawLine renderer (P (V2 (fromIntegral (x + w)) (fromIntegral y))) (P (V2 (fromIntegral (x + w)) (fromIntegral (y + h))))

-- Define colors for different zone types
zoneBorderColor :: Zone -> V4 Word8
zoneBorderColor (ZR _ _) = V4 255 0 0 255 -- red for residential
zoneBorderColor (ZI _ _) = V4 0 255 0 255 -- green for industrial
zoneBorderColor (ZC _ _) = V4 0 0 255 255 -- blue for commercial
zoneBorderColor (Eau _) = V4 0 255 255 255 -- cyan for water
zoneBorderColor (Route _) = V4 255 255 0 255 -- yellow for routes
zoneBorderColor (Admin _ _) = V4 255 0 255 255 -- magenta for administrative

getWH :: Forme -> (Int, Int)
getWH (GameData.Rectangle _ w h) = (w, h)
getWH (HSegment _ l) = (l, tailleBlocI)
getWH (VSegment _ l) = (tailleBlocI, l)

zoneCoord :: Zone -> Coord
zoneCoord (ZR forme _) = getFormeCoord forme
zoneCoord (ZI forme _) = getFormeCoord forme
zoneCoord (ZC forme _) = getFormeCoord forme
zoneCoord (Eau forme) = getFormeCoord forme
zoneCoord (Route forme) = getFormeCoord forme
zoneCoord (Admin forme _) = getFormeCoord forme

zoneForme :: Zone -> Forme
zoneForme (ZR forme _) = forme
zoneForme (ZI forme _) = forme
zoneForme (ZC forme _) = forme
zoneForme (Eau forme) = forme
zoneForme (Route forme) = forme
zoneForme (Admin forme _) = forme

data ZoneType = ZTR | ZTI | ZTC | ZTA | ZTE deriving (Show, Eq)

createStructureIfNeeded :: String -> MouseState -> [Event] -> Etat -> Map.Map String (Int, Int, Int) -> IO Etat
createStructureIfNeeded selectedStructure mouseEvent events gameState menuItems = do
  let (x, y) = Ms.getMousePosition mouseEvent
  let offset = worldOffset $ world gameState
  let gameCoord = applyOffset (C (fromIntegral x) (fromIntegral y)) (negateCoord offset)
  let isOutsideMenu = not $ any (\(_, (_, mx, my)) -> isMouseOverMenuItem (x, y) (mx, my, 100)) (Map.toList menuItems)
  if isOutsideMenu
    then return $ execState (createStructure selectedStructure gameCoord mouseEvent events) gameState
    else return gameState

createStructure :: String -> Coord -> MouseState -> [Event] -> State Etat ()
createStructure "ZoneRoute" coord mouseEvent events = do
  etat <- St.get
  when (Ms.mouseButtonPressed mouseEvent events) $
    put etat {selectionStart = Just coord}
  when (Ms.mouseButtonReleased mouseEvent events) $ do
    case selectionStart etat of
      Just startCoord -> do
        case (routeDirection etat) of
          Horizontal -> do
            let forme = routeFormeHoriz startCoord (cx coord - cx startCoord)
            if any (\zone -> collision forme (zoneForme zone)) (Map.elems $ viZones $ ville etat) && (coins etat >= 5)
              then return ()
              else put $ createZone (Route forme) 5 etat
          Vertical -> do
            let forme = routeFormeVert startCoord (cy coord - cy startCoord)
            if any (\zone -> collision forme (zoneForme zone)) (Map.elems $ viZones $ ville etat) && (coins etat >= 5)
              then return ()
              else put $ createZone (Route forme) 5 etat
      Nothing -> return ()
createStructure "ZoneR" coord mouseEvent events = handleZoneCreation ZTR coord mouseEvent events
createStructure "ZoneI" coord mouseEvent events = handleZoneCreation ZTI coord mouseEvent events
createStructure "ZoneC" coord mouseEvent events = handleZoneCreation ZTC coord mouseEvent events
createStructure "ZoneA" coord mouseEvent events = handleZoneCreation ZTA coord mouseEvent events
createStructure "ZoneE" coord mouseEvent events = handleZoneCreation ZTE coord mouseEvent events
createStructure _ _ _ _ = return ()

handleZoneCreation :: ZoneType -> Coord -> MouseState -> [Event] -> State Etat ()
handleZoneCreation zoneType coord mouseEvent events = do
  etat <- St.get
  when (Ms.mouseButtonPressed mouseEvent events) $
    put etat {selectionStart = Just coord}
  when (Ms.mouseButtonReleased mouseEvent events) $ do
    case selectionStart etat of
      Just startCoord -> do
        let forme = rectangularZone startCoord (cx coord - cx startCoord) (cy coord - cy startCoord)
        let zone = case zoneType of
              ZTR -> ZR forme []
              ZTI -> ZI forme []
              ZTC -> ZC forme []
              _ -> Eau forme
        if any (\zone -> collision forme (zoneForme zone)) (Map.elems $ viZones $ ville etat) && (coins etat >= 10)
          then return ()
          else
            put $ createZone zone 10 etat
      Nothing -> return ()

-- Spawns citizens and assigns them to homes
spawnCitizens :: Etat -> IO Etat
spawnCitizens etat = do
  let batiments = GameData.getBatiments $ ville etat
  let homes = getHomes batiments
  let randomDelays = generateRandomDelays 350  
  let etat' = execState (spawnCitizensForHomes homes  randomDelays 0) etat
  return etat'


generateRandomDelays :: Int -> [Int]
generateRandomDelays seed = randomRs (1, 1000) (mkStdGen seed)

-- Function to spawn citizens for each home
spawnCitizensForHomes :: [(BatId, Batiment)] -> [Int] -> Int -> State Etat ()
spawnCitizensForHomes [] _ _ = return ()
spawnCitizensForHomes ((batId, batiment) : xs) delays timedelay = do
  etat <- St.get
  case batiment of
    Cabane d coord maxCitizens ctz -> do
      if length ctz >= maxCitizens
        then spawnCitizensForHomes xs delays timedelay
        else do
          let (newCitoyens, newEtat) = spawnCitizensForHome batId coord maxCitizens ctz etat
          let updatedBatiment = Cabane d coord maxCitizens (ctz ++ newCitoyens)
          let (finalEtat, remainingDelays) = foldl (\(acc, d:ds) citId -> (execState (do State.scheduleEvent (State.getTime acc + 100 + d) (GameData.GoWork citId)) acc, ds)) (newEtat, delays) newCitoyens
          put $ updateBatiment updatedBatiment batId finalEtat
          spawnCitizensForHomes xs remainingDelays (timedelay + 100)
    _ -> spawnCitizensForHomes xs delays timedelay



-- Spawns citizens for a home
spawnCitizensForHome :: BatId -> Coord -> Int -> [CitId] -> Etat -> ([CitId], Etat)
spawnCitizensForHome _ _ 0 ctz etat = (ctz, etat)
spawnCitizensForHome batId coord maxCitizens ctz etat =
  if length ctz  >= maxCitizens
    then (ctz, etat)
    else
      let citoyen = Habitant coord (1000, 1000, 1000) (batId, Nothing, Nothing) GameData.Dormir
          (etatN, citId) = createCitoyen citoyen etat
       in spawnCitizensForHome batId coord (maxCitizens - 1) (ctz ++ [citId]) etatN

-- | Get all homes in the city
getHomes :: Map.Map BatId Batiment -> [(BatId, Batiment)]
getHomes batiments =
  filter
    ( \(batId, batiment) -> case batiment of
        Cabane _ _ _ _ -> True
        _ -> False
    )
    (Map.toList batiments)

-- | Check if a citizen is at one of their buildings (home, shop, work)
isCitizenAtBuilding :: Citoyen -> Map.Map BatId Batiment -> Bool
isCitizenAtBuilding (Habitant _ _ (homeId, mWorkId, mShopId) _) batiments =
  let home = Map.lookup homeId batiments
      work = case mWorkId of
        Just workId -> Map.lookup workId batiments
        Nothing -> Nothing
      shop = case mShopId of
        Just shopId -> Map.lookup shopId batiments
        Nothing -> Nothing
   in case (home, work, shop) of
        (Just (Cabane _ _ _ _), Just (Atelier _ _ _ _), Just (Epicerie _ _ _ _)) -> True
        _ -> False
isCitizenAtBuilding _ _ = False

-- Render citizens on the screen
renderCitizens :: Renderer -> TextureMap -> SpriteMap -> Ville -> Coord -> IO ()
renderCitizens renderer tmap smap ville offset = do
  let citoyens = Map.elems $ viCit ville
  let batiments = viBat ville
  mapM_ (renderCitizen renderer tmap smap offset batiments) citoyens

renderCitizen :: Renderer -> TextureMap -> SpriteMap -> Coord -> Map.Map BatId Batiment -> Citoyen -> IO ()
renderCitizen renderer tmap smap offset batiments citoyen = do
  -- unless (isCitizenAtBuilding citoyen batiments) $ do
  let (coord, spriteId) = case citoyen of
        Habitant coord _ _ _ -> (coord, SpriteId "perso") -- Use a default sprite for citizen
        _ -> (C 0 0, SpriteId "perso")
  let sprite = SM.fetchSprite spriteId smap
  let (x, y) = getXY $ applyOffset coord offset
  S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral x) (fromIntegral y))

printFirstCitizen :: Etat -> IO ()
printFirstCitizen etat = do
  let citoyens = viCit $ ville etat
  if Map.null citoyens
    then return ()
    else do
      let firstCitizen = head $ Map.elems citoyens
      putStrLn $ show firstCitizen