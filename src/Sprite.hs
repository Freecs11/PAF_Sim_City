module Sprite where
import SDL (Renderer, Texture, Rectangle(..), createTextureFromSurface, copy, destroyTexture)
import Control.Monad.IO.Class (MonadIO)

import Foreign.C.Types (CInt)

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import SDL.Vect (V2 (..), Point (..))

import SDL.Video.Renderer (Renderer, Texture, Rectangle )
import qualified SDL.Video.Renderer as R

import Data.Text (Text)

import Data.Word (Word8)

import qualified SDL 
import SDL (V4(..), Point(..), Rectangle(..))
import qualified SDL.Font as Font

import TextureMap (TextureMap, TextureId)
import qualified TextureMap as TM

import qualified Debug.Trace as T


type Area = Rectangle CInt

data Image =
  Image { textureId :: TextureId
        , sourceArea :: Area }

-- | Les lutins sont associés à des images (textures)
createImage :: TextureId -> Area -> Image
createImage txt rct = Image txt rct

data Sprite =
  Sprite { images :: Seq Image
         , current :: Int
         , destArea :: Area }

-- | création d'un lutin "vide"
createEmptySprite :: Sprite
createEmptySprite = Sprite Seq.empty 0 (mkArea 0 0 0 0) 

-- | ajouter une image à un lutin
addImage :: Sprite -> Image -> Sprite
addImage sp@(Sprite { images=is }) img = sp { images = is :|> img }

-- | changer l'image d'un lutin  (par son numéro)
changeImage :: Sprite -> Int -> Sprite
changeImage sp@(Sprite { images = imgs }) new
  | Seq.null imgs = error $ "Cannot change sprite image, no image in sprite"
  | (new < 0) || (new > Seq.length imgs) = error $ "Cannot change sprite image, bad index: " <> (show new)
  | otherwise = sp { current= new }

-- | cycler l'image d'un lutin
cycleImage :: Sprite -> Sprite
cycleImage sp@(Sprite { images = imgs, current = cur }) =
  let new = if cur == Seq.length imgs - 1 then 0 else cur + 1
  in changeImage sp new

-- | une zone rectangulaire
mkArea :: CInt -> CInt -> CInt -> CInt -> Area
mkArea x y w h = Rectangle (P (V2 x y)) (V2 w h)

-- | déplacement d'une zone  
moveArea :: Area -> CInt -> CInt -> Area
moveArea rect@(Rectangle _ wh) x y = Rectangle (P (V2 x y)) wh

-- | redimensionnement
resizeArea :: Area -> CInt -> CInt -> Area
resizeArea rect@(Rectangle p _) w h = Rectangle p (V2 w h)

-- | déplacement d'un lutin
moveTo :: Sprite -> CInt -> CInt -> Sprite
moveTo sp@(Sprite { destArea = dest }) x y = sp { destArea = moveArea dest x y }

-- | mise à l'échelle d'un lutin
scale :: Sprite -> CInt -> CInt -> Sprite
scale sp@(Sprite { destArea = dest}) w h = sp { destArea = resizeArea dest w h }

-- | récupération de l'image courante d'un lutin
currentImage :: Sprite -> Image
currentImage (Sprite imgs cur _) = Seq.index imgs cur

-- | échelle par défaut du lutin, en fonction de son image courante
defaultScale :: Sprite -> Sprite
defaultScale sp = case currentImage sp of
                    (Image _ (Rectangle _ (V2 w h))) -> scale sp w h

-- | affichage d'un lutin sur le `renderer` SDL2
displaySprite :: Renderer -> TextureMap -> Sprite -> IO ()
displaySprite rdr tmap sp@(Sprite imgs cur dest) =
  case currentImage sp of
    (Image tid src) -> do
      let txt = TM.fetchTexture tid tmap
      R.copy rdr txt Nothing (Just dest)
      
-- | affichage d'un texte sur le `renderer` SDL2
displayText :: Renderer -> Font.Font -> Text -> V2 CInt -> V4 Word8 -> IO ()
displayText rdr font txt pos color = do
  surface <- Font.solid font color txt
  texture <- createTextureFromSurface rdr surface
  surfaceSize <- R.surfaceDimensions surface
  let destRect = Rectangle (P pos) surfaceSize
  copy rdr texture Nothing (Just destRect)
  destroyTexture texture
  SDL.freeSurface surface

