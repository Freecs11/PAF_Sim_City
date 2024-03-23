
module TextureMap where

import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map as M

import SDL
import qualified SDL.Video.Renderer as R

newtype TextureId = TextureId String
  deriving (Eq, Ord)

instance Show TextureId where
  show (TextureId tid) = show tid

type TextureMap = Map TextureId Texture

-- | Création d'une map de textures vide
createTextureMap :: TextureMap
createTextureMap = M.empty

-- | Ajout d'une texture
addTexture :: TextureId -> Texture -> TextureMap -> TextureMap
addTexture tid txt tmap =
  M.insertWithKey (\_ _ _ -> error $ "addTexture - Texture '" <> (show tid) <> "' already in texture map.")
  tid txt tmap

-- | Chargement d'une texture à partir d'un fichier
loadTexture  :: Renderer -> FilePath -> TextureId -> TextureMap -> IO TextureMap
loadTexture rdr path tid tmap = do
  srf <- R.loadBMP path
  txt <- createTextureFromSurface rdr srf
  R.freeSurface srf
  pure $ addTexture tid txt tmap 

-- | Récupération d'une texture à partir de sa clé
fetchTexture :: TextureId -> TextureMap -> Texture
fetchTexture tid tmap = case M.lookup tid tmap of
                           Nothing -> error $ "fetchTexture - No such texture: " <> (show tid)
                           Just txt -> txt

-- | Destruction d'une texture (récupération mémoire)
destroyTexture :: TextureId -> TextureMap -> IO TextureMap
destroyTexture tid tmap = case M.lookup tid tmap of
                            Nothing -> error $ "destroyTexture - No such texture : " <> (show tid) 
                            Just txt -> do
                              let tmap' = M.delete tid tmap
                              R.destroyTexture txt
                              pure $ tmap'


-- | Destruction de la map des textures
destroyTextureMap :: TextureMap -> IO TextureMap
destroyTextureMap tmap =
  let tids = fmap fst $ M.toList tmap
  in do
    forM_ tids (\tid -> TextureMap.destroyTexture tid tmap)
    pure createTextureMap

