
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

-- A supprimer 

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft (GameState x y s) = GameState (x - s) y s

moveRight :: GameState -> GameState
moveRight (GameState x y s) = GameState (x + s) y s
                              
moveUp :: GameState -> GameState
moveUp (GameState x y s) = GameState x (y - s) s

moveDown :: GameState -> GameState
moveDown (GameState x y s) = GameState x (y + s) s

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate


  -- les évenement de la souris sont traités dans le main ( je n'ai pas eu le temps de les mettre ici , puisque c'est des IO() ) 
  
