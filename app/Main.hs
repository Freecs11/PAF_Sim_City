{-# LANGUAGE OverloadedStrings #-}
module Main where



import qualified Data.Map as Map

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )
import SDL
import qualified SDL.Font as Font
import Linear (V4(..), V2(..))
import SDL.Time (time, delay)
import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM
import Control.Monad (foldM)

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard (..))
import qualified Keyboard as K
import Mouse (MouseState (..))
import qualified Mouse as Ms

import qualified Debug.Trace as T
import Data.Text (pack)


import qualified GameData as GameData
import GameData (Coord (..), Batiment (..), ZonId (..), BatId (..), CitId (..) , Citoyen (..), Zone (..), Ville (..) , Etat (..) , Selection (..) )
import qualified Batiments as Bat

import qualified State as State 

import Data.Word (Word8)


-- TO BE MODIFIED --------------------------------------------------------------

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "grass") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "grass") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "grass") sprite smap
  return (tmap', smap')

-- | Load the perso sprite
loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')


-- | Load the building sprites
loadBuildings :: Renderer -> [ (TextureId, FilePath) ] -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBuildings rdr [] tmap smap = return (tmap, smap)
loadBuildings rdr ((tid, path):xs) tmap smap = do
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
menu = return $ Map.fromList [("Cabane", (100, 1100, 80)), 
                              ("Epicerie", (200, 1100, 95)), 
                              ("Police", (300, 1100, 110)), 
                              ("Commissariat", (400, 1100, 125)),  
                              ("Railway", (100, 1100, 140))
                              ]

renderMenuItems :: Renderer -> Font.Font -> Map.Map String (Int, Int, Int) -> IO ()
renderMenuItems renderer font menuItems = do
  -- on render chaque item du menu
  Map.traverseWithKey (\name (cost, x, y) -> 
    S.displayText renderer font (pack $ name ++ " : " ++ show cost) (V2 (fromIntegral x) (fromIntegral y)) (V4 255 255 255 255)) menuItems
  return ()

zonesMenu :: IO (Map.Map String (Int, Int))
zonesMenu = return $ Map.fromList [("ZoneRoute", (1100, 170)), 
                                   ("ZoneR", (1100, 195)),
                                    ("ZoneI", (1100, 220)),
                                    ("ZoneC", (1100, 245)),
                                    ("ZoneA", (1100, 270)),
                                    ("ZoneE", (1100, 295))
                                    ]

renderZonesMenuItems :: Renderer -> Font.Font -> Map.Map String (Int, Int) -> IO ()
renderZonesMenuItems renderer font menuItems = do
  -- on render chaque item du menu
  Map.traverseWithKey (\name (x , y ) -> 
    S.displayText renderer font (pack $ name) (V2 (fromIntegral x) (fromIntegral y)) (V4 255 255 255 255)) menuItems
  return ()


-- check if the mouse is over a menu item
isMouseOverMenuItem :: (Int, Int) -> (Int, Int, Int) -> Bool
isMouseOverMenuItem (x, y) (x', y', w) = x > x' && x < x' + w && y > y' && y < y' + 20

-- | Get the texture id of a sprite
getTextureId :: Sprite -> TextureId
getTextureId sprite = case S.currentImage sprite of
  S.Image tid _ -> tid




main :: IO ()
main = do
  initializeAll
  Font.initialize
  window <- createWindow "Sim City" $ defaultWindow { windowInitialSize = V2 1280 680 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  -- (tmap, smap) <- loadBackground renderer "assets/grass.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp"  TM.createTextureMap SM.createSpriteMap

  -- background color is black for now ( will load dirt texture later)
  loadBackgroundColor renderer (V4 0 0 0 255)

  -- liste d'éléments (Texture ID, FilePath) à charger
  let buildings = [ (TextureId "Cabane", "assets/residuel.bmp"),
                    (TextureId "Atelier", "assets/atelier.bmp"),
                    (TextureId "Epicerie", "assets/epicerie.bmp"),
                    (TextureId "Commissariat", "assets/police.bmp") ]


  let startCoins = 3000
  let taxesCitizens = 100
  -- initialisation de l'état du jeu , batiment selectionné par défaut est les routes
  let gameState0 = State.initialiseState startCoins

  let retrEvent = GameData.TaxRetreival taxesCitizens 
  -- on schedule un évenement pour prélever des taxes sur les citoyens tous les 1000000 unités de temps  
  let gameState = State.scheduleEvent 1000000 retrEvent gameState0 
  

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

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> Etat -> MouseState -> Font.Font ->  Map.Map String (Int, Int, Int) -> Map.Map String (Int, Int) -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState mouse font menuItems zonesMenuItems= do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mouse' = Ms.handleMouseEvents events mouse  -- Update mouse state based on events
  clear renderer
  
  -- S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "grass") smap)

  let selectedBuilding  = ( case State.getSelectedBuilding gameState of
                            BuildingType building -> building
                            ZoneType zone -> zone
                            None -> "None" )
  -- display the selected building on the screen top left
  S.displayText renderer font ("Selected building: " <> pack selectedBuilding) (V2 3 5) (V4 255 255 255 255)

  -- display menu
  renderMenuItems renderer font menuItems

  -- display zones menu
  renderZonesMenuItems renderer font zonesMenuItems

  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor (((1.0 / frameRate) - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"

  let updatedMenuItemsIOcall = updateMenuItems menuItems zonesMenuItems selectedBuilding mouse' events
  
  -- on execute l'IO pour obtenir les valeurs
  (updatedMenuItems, currentSelectedBuilding) <- updatedMenuItemsIOcall


  -- updatedMenuItems <- updatedMenuItems
  -- currentSelectedBuilding <- currentSelectedBuilding

  putStrLn $ "Selected building: " <> currentSelectedBuilding


  --- update du game state
  let gameState' = State.processEvents (State.getTime gameState) gameState
  let gameState'' = State.updateSelectedBuilding currentSelectedBuilding gameState'

  
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState'' mouse' font updatedMenuItems  zonesMenuItems)


updateMenuItems :: Map.Map String (Int, Int, Int) -> Map.Map String (Int, Int) -> String -> MouseState -> [Event] -> IO ((Map.Map String (Int, Int, Int), String))
updateMenuItems menuItems zonesMenuItems selectedBuilding mouseEvent events = do
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
                    return (Map.update (\(c, x'', y'') -> Just (c - 1, x'', y'')) itemName menuItems , itemName)
                else if not (Map.null clickedZoneItem)
                    then do
                        let selectedZoneItem = head $ Map.toList clickedZoneItem
                        let (zoneName, (x', y')) = selectedZoneItem
                        putStrLn $ "Clicked on zone: " <> zoneName -- Display the name of the clicked zone
                        return (menuItems, zoneName)
                    else return (menuItems, selectedBuilding) -- No item was clicked
        else return (menuItems, selectedBuilding) -- No mouse button was pressed


-- pose une zone sur la carte en drag la souris pour définir la zone et en relachant la souris pour valider la zone

