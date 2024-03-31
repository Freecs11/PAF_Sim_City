module Batiments where

import GameData
import Sprite
import SDL
import TextureMap
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CInt)
import Data.Sequence (Seq (..) )
import qualified Data.Sequence as Seq
import SDL.Vect (V2 (..), Point (..))
import SDL.Video.Renderer (Renderer, Texture, Rectangle )
import qualified SDL.Video.Renderer as R
import Data.Text (Text)
import Data.Word (Word8)
import qualified SDL
import SDL (V4(..), Point(..), Rectangle(..) )
import qualified SDL.Font as Font
import qualified Debug.Trace as T
import qualified Data.Map as Map
import Data.Map (Map)



-- batiments functions and invariants/pre/post conditions



-- SDL specific functions to draw buildings and their states 

-- | draw a building

-- Import necessary modules


-- Function to create a sprite for a given building type
createBuildingSprite :: Batiment -> TextureMap -> IO Sprite
createBuildingSprite building textureMap = do
    -- Determine the texture id based on the building type
    let textureId = case building of
            Cabane _ _ _ _ -> TextureId "Cabane"
            Atelier _ _ _ _ -> TextureId "Atelier"
            Epicerie _ _ _ _ -> TextureId "Epicerie"
            Commissariat _ _ -> TextureId "Commissariat"
    -- Fetch the texture based on the texture id from the texture map
    -- debug
    putStrLn $ "Fetching texture: " ++ show textureId
    let texture = fetchTexture textureId textureMap
    let defaultPosition = mkArea 0 0 0 0 -- to Modify position
    let image = createImage textureId defaultPosition
    let sprite = createEmptySprite `addImage` image
    return sprite

-- | get the sprite of a building
fetchBuildingSprite :: Batiment -> TextureMap -> IO Sprite
fetchBuildingSprite building textureMap = 
    case building of
    Cabane _ _ _ _ -> fetchTextureSprite (TextureId "Cabane") textureMap
    Atelier _ _ _ _ -> fetchTextureSprite (TextureId "Atelier") textureMap
    Epicerie _ _ _ _ -> fetchTextureSprite (TextureId "Epicerie") textureMap
    Commissariat _ _ -> fetchTextureSprite (TextureId "Commissariat") textureMap

-- | get the sprite of a building
fetchTextureSprite :: TextureId -> TextureMap -> IO Sprite
fetchTextureSprite tid tmap = let texture = fetchTexture tid tmap
                                  defaultPosition = mkArea 0 0 0 0
                                  image = createImage tid defaultPosition
                              in 
                               return(createEmptySprite `addImage` image )

-- get the map of buildings from a city
getBatiments :: Ville -> Map BatId Batiment
getBatiments Ville { viZones = zones } = Map.fromList $ concatMap getBatimentsFromZone $ Map.toList zones
    where
        getBatimentsFromZone :: (ZonId, Zone) -> [(BatId, Batiment)]
        getBatimentsFromZone (zoneId, zone) = case zone of
            ZR _ batiments -> zip (map BatId [0..]) batiments
            ZI _ batiments -> zip (map BatId [0..]) batiments
            ZC _ batiments -> zip (map BatId [0..]) batiments
            _ -> []



    








