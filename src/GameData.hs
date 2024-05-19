module GameData where

import Data.Map (Map)
import qualified Data.Map as Map


--- all the data types used in the game are defined here 
-- le GameData contient tous les types et donc est importé par la plupart des modules


-- Coordinates of a point in the map
data Coord = C {cx :: Int ,
                cy :: Int}
                deriving (Show , Eq)

instance Ord Coord where
    compare (C x1 y1) (C x2 y2) = compare (x1 , y1) (x2 , y2)

-- -- Distance between two points
-- distance :: Coord -> Coord -> Int
-- distance (C x1 y1) (C x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Form of a building or a zone
data Forme = HSegment Coord Int
            | VSegment Coord Int
            | Rectangle Coord Int Int
            deriving (Show , Eq)


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
data Batiment = Cabane Forme Coord Int [CitId]
                | Atelier Forme Coord Int [CitId]
                | Epicerie Forme Coord Int [CitId]
                | Commissariat Forme Coord
                deriving (Show , Eq)


-- Zone data type
data Zone = Eau Forme
        | Route Forme
        | ZR Forme [BatId]
        | ZI Forme [BatId]
        | ZC Forme [BatId]
        | Admin Forme BatId
        deriving (Show, Eq)

-- MAP BATIMENT to BATid 

-- Occupation data type (what a citizen is doing at a given time)
data Occupation = Dormir | Travailler | FaireCourses | SeDeplacer Coord
                    deriving (Show , Eq)



-- Citoyen data type
data Citoyen = Immigrant Coord (Int , Int , Int) Occupation
                | Habitant Coord (Int , Int , Int) (BatId , Maybe BatId , Maybe BatId) Occupation
                | Emigrant Coord Occupation
                deriving (Show , Eq)



-- Ville data type
data Ville = Ville {
            viBat :: Map BatId Batiment,
            viCit :: Map CitId Citoyen,
            viZones :: Map ZonId Zone            
            }
            deriving (Show , Eq)


data Event = Move Coord CitId -- évenement pour déplacer un citoyen vers un Coord ( normalement celui d'un batiment)
           | StartWork CitId -- commence le travaille , va rajouté dans l'état le temps de fin de travail du citoyen comme currentTime + temps de travail
           -- comme ça si le temps de travail est fini on peut le faire rentrer chez lui en rajoutant un autre évenement GoHome
           | GoShopping CitId -- commence le shopping, meme principe que StartWork
           | GoHome CitId -- rentre chez lui , il signifie que le citoyen a fini son travail ou son shopping et qu'il rentre chez lui directement ( suivant le chemin le plus court)
           | UpdateNeeds CitId -- met à jour les besoins du citoyen ( du genre faim, soif, etc.  qui décroit avec le temps) , donc on va rajouter un évenement pour mettre à jour les besoins de chaque citoyen à chaque tour de boucle
           deriving (Eq, Show)

-- on va stocker les évenements à faire à un temps donné , 
-- ce que j'imagine c'est dans la boucle de jeu on va regarder si on a des évenements à faire à currentTime et on les fait
-- et on les enlève de la liste des évenements
data Etat =  Etat {
        ville :: Ville,
        coins :: Int,
        carte :: Map Coord (BatId, [CitId]), -- on va stocker les batiments et les citoyens à chaque coordonnée
        currentTime :: Int , -- temps actuel du jeu , utilise un entier pour l'instant
        events :: Map Int [Event] 
    }
    deriving (Show, Eq)

getCarte :: Etat -> Map Coord (BatId, [CitId])
getCarte (Etat {carte = c}) = c