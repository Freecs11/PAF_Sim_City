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

import Model (GameState)
import qualified Model as M

import qualified GameData as GameData
import GameData (Coord (..), Batiment (..), ZonId (..), BatId (..), CitId (..) , Citoyen (..), Zone (..), Ville (..))
import qualified Batiments as Bat

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




menu :: IO (Map.Map String (Int, Int, Int))
menu = return $ Map.fromList [("Cabane", (100, 500, 80)), 
                              ("Epicerie", (200, 500, 95)), 
                              ("Police", (300, 500, 110)), 
                              ("Commissariat", (400, 500, 125)), 
                              ("Road", (50, 500, 140)), 
                              ("Railway", (100, 500, 155))]

renderMenuItems :: Renderer -> Font.Font -> Map.Map String (Int, Int, Int) -> IO ()
renderMenuItems renderer font menuItems = do
  -- on render chaque item du menu
  Map.traverseWithKey (\name (cost, x, y) -> 
    S.displayText renderer font (pack $ name ++ " : " ++ show cost) (V2 (fromIntegral x) (fromIntegral y)) (V4 255 255 255 255)) menuItems
  return ()


-- check if the mouse is over a menu item
isMouseOverMenuItem :: (Int, Int) -> (Int, Int, Int) -> Bool
isMouseOverMenuItem (x, y) (x', y', w) = x > x' && x < x' + w && y > y' && y < y' + 20




-- | Get the texture id of a sprite
getTextureId :: Sprite -> TextureId
getTextureId sprite = case S.currentImage sprite of
  S.Image tid _ -> tid


getSelectedBuilding :: TextureMap -> SpriteMap -> String -> Batiment 
getSelectedBuilding tmap smap selectedBuilding = 
  case selectedBuilding of
    "Cabane" -> Cabane (GameData.Rectangle (C 0 0) 100 100) (C 0 0) 100 []
    "Atelier" -> Atelier (GameData.Rectangle (C 0 0) 100 100) (C 0 0) 100 []
    "Epicerie" -> Epicerie (GameData.Rectangle (C 0 0) 100 100) (C 0 0) 100 []
    "Commissariat" -> Commissariat (GameData.Rectangle (C 0 0) 100 100) (C 0 0)
    _ -> Cabane (GameData.Rectangle (C 0 0) 100 100) (C 0 0) 100 []




main :: IO ()
main = do
  initializeAll
  Font.initialize
  window <- createWindow "Sim City" $ defaultWindow { windowInitialSize = V2 1280 680 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/grass.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap

  -- liste d'éléments (Texture ID, FilePath) à charger
  let buildings = [ (TextureId "Cabane", "assets/residuel.bmp"),
                    (TextureId "Atelier", "assets/atelier.bmp"),
                    (TextureId "Epicerie", "assets/epicerie.bmp"),
                    (TextureId "Commissariat", "assets/police.bmp") ]




  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  let mouse = Ms.createMouseState

  -- batiment par défaut sélectionné
  let selectedBuilding = "house"  -- type String



  -- Load menu items
  menuItems <- menu

  -- faut télécharger SDL2_ttf pour que ça marche 
  font <- Font.load "assets/Nexa-Heavy.ttf" 15

  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd gameState mouse font menuItems selectedBuilding []

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> MouseState -> Font.Font -> Map.Map String (Int, Int, Int) -> String -> [Batiment] -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState mouse font menuItems selectedBuilding batiments = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mouse' = Ms.handleMouseEvents events mouse  -- Update mouse state based on events
  clear renderer
  
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "grass") smap)

  -- display the selected building on the screen top left
  S.displayText renderer font ("Selected building: " <> pack selectedBuilding) (V2 3 5) (V4 255 255 255 255)

  -- display menu
  renderMenuItems renderer font menuItems


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

  let updatedMenuItemsIOcall = updateMenuItems menuItems selectedBuilding mouse' events
  
  -- on execute l'IO pour obtenir les valeurs
  (updatedMenuItems, currentSelectedBuilding) <- updatedMenuItemsIOcall


  -- updatedMenuItems <- updatedMenuItems
  -- currentSelectedBuilding <- currentSelectedBuilding

  putStrLn $ "Selected building: " <> currentSelectedBuilding


  --- update du game state
  let gameState' = M.GameState (M.city gameState) (M.coins gameState) currentSelectedBuilding
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' mouse' font updatedMenuItems currentSelectedBuilding batiments)


updateMenuItems :: Map.Map String (Int, Int, Int) -> String -> MouseState -> [Event] -> IO ( (Map.Map String (Int, Int, Int), String) )
updateMenuItems menuItems selectedBuilding mouseEvent events = do
    if Ms.mouseButtonPressed mouseEvent events
        then do
            let (x, y) = Ms.getMousePosition mouseEvent
            let clickedItem = Map.filterWithKey (\_ (cost, x', y') -> x > fromIntegral x' && x < fromIntegral (x' + 100) && y > fromIntegral y' && y < fromIntegral (y' + 20)) menuItems
            if not (Map.null clickedItem)
                then do
                    let selectedItem = head $ Map.toList clickedItem
                    let (itemName, (cost, x', y')) = selectedItem
                    putStrLn $ "Clicked on: " <> itemName -- on affiche le nom du batiment cliqué sur le stdout
                    -- à modifier/supprimer ( le faire quand on place le batiment sur la map)
                    return (Map.update (\(c, x'', y'') -> Just (c - 1, x'', y'')) itemName menuItems , itemName)
                else return (menuItems, selectedBuilding) -- aucun batiment n'a été cliqué
        else return (menuItems, selectedBuilding) -- aucun bouton de la souris n'a été pressé


