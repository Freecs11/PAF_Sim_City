module GameData where

import Control.Monad (when)
import Control.Monad.State (MonadState (get, put), State, evalState, execState, modify)
import Data.Map (Map)
import qualified Data.Map as Map

--- all the data types used in the game are defined here
-- le GameData contient tous les types et donc est importé par la plupart des modules

-- Coordinates of a point in the map
data Coord = C
  { cx :: Int,
    cy :: Int
  }
  deriving (Show, Eq)

instance Ord Coord where
  compare (C x1 y1) (C x2 y2) = compare (x1, y1) (x2, y2)


-- Form of a building or a zone
data Forme
  = HSegment Coord Int
  | VSegment Coord Int
  | Rectangle Coord Int Int
  deriving (Show, Eq)

-- Building types
newtype ZonId = ZonId Int

newtype BatId = BatId Int

newtype CitId = CitId String

instance Show ZonId where
  show (ZonId i) = show "Zone " ++ show i

instance Show BatId where
  show (BatId i) = show "Batiment " ++ show i

instance Show CitId where
  show (CitId i) = show "Citoyen " ++ i

instance Eq ZonId where
  (ZonId i) == (ZonId j) = i == j

instance Eq BatId where
  (BatId i) == (BatId j) = i == j

instance Eq CitId where
  (CitId i) == (CitId j) = i == j

instance Ord ZonId where
  compare (ZonId i) (ZonId j) = compare i j

instance Ord BatId where
  compare (BatId i) (BatId j) = compare i j

instance Ord CitId where
  compare (CitId i) (CitId j) = compare i j

-- Batiment data type
data Batiment
  = Cabane Forme Coord Int [CitId] 
  | Atelier Forme Coord Int [CitId]
  | Epicerie Forme Coord Int [CitId]
  | Commissariat Forme Coord
  deriving (Show, Eq)

-- Zone data type
data Zone
  = Eau Forme
  | Route Forme
  | ZR Forme [BatId] 
  | ZI Forme [BatId] 
  | ZC Forme [BatId] 
  | Admin Forme BatId 
  deriving (Show, Eq)

-- MAP BATIMENT to BATid

-- Occupation data type (what a citizen is doing at a given time)
data Occupation = Dormir | Travailler | FaireCourses | SeDeplacer Coord
  deriving (Show, Eq)

-- Citoyen data type
data Citoyen
  = Immigrant Coord (Int, Int, Int) Occupation
  | Habitant Coord (Int, Int, Int) (BatId, Maybe BatId, Maybe BatId) Occupation
  | Emigrant Coord Occupation
  deriving (Show, Eq)

-- Ville data type
data Ville = Ville
  { viBat :: Map BatId Batiment,
    viCit :: Map CitId Citoyen,
    viZones :: Map ZonId Zone
  }
  deriving (Show, Eq)

-- Getters for Ville
getZones :: Ville -> Map ZonId Zone
getZones (Ville {viZones = z}) = z
getBatiments :: Ville -> Map BatId Batiment
getBatiments (Ville {viBat = b}) = b
getCitoyens :: Ville -> Map CitId Citoyen
getCitoyens (Ville {viCit = c}) = c

-- PathfindingRequest est utilisé pour demander un pathfinding au système de pathfinding (utilisé comme un système de 'batching' pour éviter de lancer A* pour de nombreux citoyens en même temps)
data PathfindingRequest = PathfindingRequest CitId Coord deriving (Eq, Show)

-- Queue pour les pathfinding requests
type PathfindingQueue = [PathfindingRequest] 

-- Event data type, utilisé pour géré les évenements dans le jeu
data Event
  = Move PathfindingQueue
  | GoWork CitId -- commence le travaille , va rajouté dans l'état le temps de fin de travail du citoyen comme currentTime + temps de travail
  -- comme ça si le temps de travail est fini on peut le faire rentrer chez lui ou autre en rajoutant un autre évenement GoHome
  | GoShopping CitId -- commence le shopping, meme principe que StartWork
  | GoHome CitId -- rentre chez lui , il signifie que le citoyen a fini son travail ou son shopping et qu'il rentre chez lui directement ( suivant le chemin le plus court)
  | UpdateMoney CitId -- met à jour l'argent du citoyen
  | UpdateHunger CitId -- met à jour la faim du citoyen
  | UpdateFatigue CitId -- met à jour la fatigue du citoyen
  | UpdateCitizens -- met à jour les citoyens
  | TaxRetreival Int -- évenement pour prélever une taxe sur les citoyens, va juste rajouter des coins à l'état
  | FollowPath [Coord] CitId -- évenement pour faire suivre un chemin à un citoyen
  | Moving Coord CitId -- move déclenche le pathfinding et moving fait le mouvement
  | AssignBuildingstoCitizens -- assigne les batiments aux citoyens ( leurs travail, magasin) , la maison est déjà assignée à la création du citoyen
  | PlaceRoute Coord  -- place une route à la coordonnée donnée
  deriving (Eq, Show)

-- utilisation de la selection pour savoir si on a sélectionné un batiment ou une zone ou rien ( ce que le joueur a sélectionné en dernier)
data Selection = ZoneType String | BuildingType String | None
  deriving (Show, Eq)

-- world offset pour déplacer la carte (différencé les coords de la carte et les coords de l'écran)
data World = World {worldOffset :: Coord} deriving (Show, Eq)

-- direction de la route (pour le placement des routes) R pour switcher entre horizontal et vertical
data RouteDirection = Horizontal | Vertical deriving (Eq, Show)

-- on va stocker les évenements à faire à un temps donné ,
-- ce que j'imagine c'est dans la boucle de jeu on va regarder si on a des évenements à faire à currentTime et on les fait
-- et on les enlève de la liste des évenements
data Etat = Etat
  { ville :: Ville,
    coins :: Int,
    carte :: Map Coord (BatId, [CitId]), -- on va stocker les batiments et les citoyens à chaque coordonnée
    currentTime :: Int, -- temps actuel du jeu , utilise un entier pour l'instant
    events :: Map Int [Event],
    selection :: Selection,
    world :: World,
    routeDirection :: RouteDirection,
    selectionStart :: Maybe Coord,
    pathCache :: Map (Coord, Coord) (Int, [Coord]), -- ajoute un cache pour les paths
    pathfindingQueue :: PathfindingQueue
    -- mouseHeld :: Bool
  }
  deriving (Show, Eq)

-- Getters for Etat
getCarte :: Etat -> Map Coord (BatId, [CitId])
getCarte (Etat {carte = c}) = c
getBatimentsIDs :: Ville -> [BatId]
getBatimentsIDs (Ville {viBat = b}) = Map.keys b
