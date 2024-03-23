module Mouse where

import SDL
import Linear.V2 (V2(..))
import SDL (Event(..), EventPayload(..), mouseMotionEventPos, Point(..))

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

import Foreign.C.Types (CInt)


import Sprite (Sprite)
import qualified Sprite as S


-- type pour la position de la souris -- le meme que celui de SDL
type MouseState = Point V2 Int

-- | création de la structure d'état de la souris (vide)
createMouseState :: MouseState
createMouseState = P (V2 0 0)

-- | prise en compte des événements SDL2 pour mettre à jour l'état de la souris
handleMouseMotion :: Event -> MouseState -> MouseState
handleMouseMotion event (P (V2 x y)) =
  case eventPayload event of -- eventPayload est une fonction de SDL qui extrait le contenu de l'evenement
    MouseMotionEvent mouseMotionEvent ->
      let V2 x' y' = fmap fromIntegral $ unP (mouseMotionEventPos mouseMotionEvent)
      in P (V2 x' y')
    _ -> P (V2 x y)
    
handleMouseEvents :: [Event] -> MouseState -> MouseState
handleMouseEvents events mouse = foldl' (flip handleMouseMotion) mouse events

mouseOnPosition :: MouseState -> (Int, Int) -> Sprite -> Bool
mouseOnPosition (P (V2 x y)) (x', y') sprite =
  let (Rectangle (P (V2 x1 y1)) (V2 w h)) = S.destArea sprite
  in x >= x' && x <= x' + fromIntegral w && y >= y' && y <= y' + fromIntegral h
  -- fromIntegral pour convertir les CInt en Int


-- on regarde si un button quelconque de la souris est pressé
mouseButtonPressed :: MouseState -> [Event] -> Bool
mouseButtonPressed mouse events =
  let buttonPressed event = case eventPayload event of
                              MouseButtonEvent mouseButtonEvent -> 
                                mouseButtonEventMotion mouseButtonEvent == Pressed
                              _ -> False
  in any buttonPressed events 

