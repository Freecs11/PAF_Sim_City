{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Concurrent (threadDelay)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard (..))
import qualified Keyboard as K
import Mouse (MouseState (..))
import qualified Mouse as Ms

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M


-- TO BE MODIFIED --------------------------------------------------------------

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "grass") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "grass") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "grass") sprite smap
  return (tmap', smap')

loadPerso :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadPerso rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "perso") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "perso") (S.mkArea 0 0 100 100)
  let smap' = SM.addSprite (SpriteId "perso") sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Sim City" $ defaultWindow { windowInitialSize = V2 640 480 }
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/grass.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement du personnage
  (tmap', smap') <- loadPerso renderer "assets/perso.bmp" tmap smap
  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  let mouse = Ms.createMouseState
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap' smap' kbd gameState mouse

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> MouseState -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState mouse = do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mouse' = Ms.handleMouseEvents events mouse  -- Update mouse state based on events
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "grass") smap)
  --- display perso 
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId "perso") smap)
                                 (fromIntegral (M.persoX gameState))
                                 (fromIntegral (M.persoY gameState)))
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

  -- si un bouton de la souris est pressé et que le curseur est sur le personnage
  if Ms.mouseOnPosition mouse' (M.persoX gameState, M.persoY gameState) (SM.fetchSprite (SpriteId "perso") smap)
    && Ms.mouseButtonPressed mouse' events
    then putStrLn "Touché !"
    else return ()
    
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  ---
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' mouse')
