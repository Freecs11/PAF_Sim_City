{-# LANGUAGE OverloadedStrings #-}

module Main where

import Batiments
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, unless, when)
import Control.Monad.State (State, evalState, execState, get, modify, put, runState)
import qualified Control.Monad.State as St
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Word (Word8)
import qualified Debug.Trace as T
import Foreign.C.Types (CInt (..))
import Formes
import GameData
import GameData (BatId (..), Batiment (..), CitId (..), Citoyen (..), Coord (..), Etat (..), Selection (..), Ville (..), World (..), ZonId (..), Zone (..), getCarte)
import qualified GameData as GameData
import Keyboard (Keyboard (..))
import qualified Keyboard as K
import Linear (V2 (..), V4 (..))
import Mouse (MouseState (..), getMousePosition, mouseButtonPressed)
import qualified Mouse as Ms
import SDL
import qualified SDL.Font as Font
import SDL.Time (delay, time)
import Sprite (Sprite)
import qualified Sprite as S
import SpriteMap (SpriteId (..), SpriteMap)
import qualified SpriteMap as SM
import qualified State as State
import TextureMap (TextureId (..), TextureMap)
import qualified TextureMap as TM

windowsWidth :: CInt
windowsWidth = 1280

windowsHeight :: CInt
windowsHeight = 680

tailleBloc :: CInt
tailleBloc = 50

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
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
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
      [ ("Cabane", (100, 1100, 80)),
        ("Epicerie", (200, 1100, 95)),
        ("Police", (300, 1100, 110)),
        ("Commissariat", (400, 1100, 125)),
        ("Railway", (100, 1100, 140))
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
  S.displaySprite renderer tmap (S.moveTo sprite (fromIntegral x) (fromIntegral y))

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
  -- on schedule un évenement pour prélever des taxes sur les citoyens tous les 1000000 unités de temps
  let gameState = execState (State.scheduleEvent 1000000 retrEvent) gameState0

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

  -- mais ça après
  --- display background
  renderBackgroundMap renderer tmap smap (worldOffset $ world gameStateW)

  -- Display the selected building on the screen top left
  S.displayText renderer font ("Selected building: " <> pack selectedBuilding) (V2 3 5) (V4 255 255 255 255)

  -- Render buildings with world offset
  renderBuildings renderer tmap smap (ville gameStateW) (worldOffset $ world gameStateW)

  -- Display menu
  renderMenuItems renderer font menuItems
  -- Display zones menu
  renderZonesMenuItems renderer font zonesMenuItems

  let isKeyPressed = K.keypressed KeycodeZ kbd'
  when isKeyPressed $ putStrLn "Z key pressed"

  present renderer

  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  let deltaTime = endTime - startTime

  -- Update menu items based on mouse events
  (updatedMenuItems, currentSelectedBuilding) <- updateMenuItems menuItems zonesMenuItems selectedBuilding mouse' events

  putStrLn $ "Selected building: " <> currentSelectedBuilding

  -- Check if a building needs to be created
  gameState'' <- createBuildingIfNeeded currentSelectedBuilding mouse' events gameStateW menuItems

  -- Update game state using State monad
  let gameState' =
        execState
          ( do
              State.processEvents (State.getTime gameState'')
              State.updateSelectedBuilding currentSelectedBuilding
          )
          gameState''

  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' mouse' font updatedMenuItems zonesMenuItems)

createBuildingIfNeeded :: String -> MouseState -> [SDL.Event] -> Etat -> Map.Map String (Int, Int, Int) -> IO Etat
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
  let ville' = (ville etat) {viBat = Map.insert (BatId (Map.size (viBat (ville etat)) + 1)) (Cabane (GameData.Rectangle coord 50 50) coord 10 []) (viBat (ville etat))}
  put etat {ville = ville'}
createBuilding "Atelier" coord = do
  etat <- St.get
  let ville' = (ville etat) {viBat = Map.insert (BatId (Map.size (viBat (ville etat)) + 1)) (Atelier (GameData.Rectangle coord 50 50) coord 10 []) (viBat (ville etat))}
  put etat {ville = ville'}
createBuilding "Epicerie" coord = do
  etat <- St.get
  let ville' = (ville etat) {viBat = Map.insert (BatId (Map.size (viBat (ville etat)) + 1)) (Epicerie (GameData.Rectangle coord 50 50) coord 10 []) (viBat (ville etat))}
  put etat {ville = ville'}
createBuilding "Commissariat" coord = do
  etat <- St.get
  let ville' = (ville etat) {viBat = Map.insert (BatId (Map.size (viBat (ville etat)) + 1)) (Commissariat (GameData.Rectangle coord 50 50) coord) (viBat (ville etat))}
  put etat {ville = ville'}
createBuilding _ _ = return ()

updateMenuItems :: Map.Map String (Int, Int, Int) -> Map.Map String (Int, Int) -> String -> MouseState -> [SDL.Event] -> IO (Map.Map String (Int, Int, Int), String)
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
          putStrLn $ "Clicked on: " <> itemName -- Display the name of the clicked building
          return (Map.update (\(c, x'', y'') -> Just (c - 1, x'', y'')) itemName menuItems, itemName)
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

-- ville is the new coords of the buildings/citizens/zones
updateCarteWorldMoving :: Ville -> Etat -> Map.Map Coord (BatId, [CitId])
updateCarteWorldMoving ville etat =
  let carte = getCarte etat
      batiments = viBat ville
   in Map.foldrWithKey (\batId bat carte -> updateCarte (getBatimentCoord bat) batId carte) carte batiments

updateCarte :: Coord -> BatId -> Map.Map Coord (BatId, [CitId]) -> Map.Map Coord (BatId, [CitId])
updateCarte coord batId carte =
  let (batId', citIds) = case Map.lookup coord carte of
        Just (batId', citIds) -> (batId', citIds)
        Nothing -> (BatId 0, [])
   in Map.insert coord (batId, citIds) carte
