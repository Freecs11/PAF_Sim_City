module GameData where

import Data.Map (Map)
import qualified Data.Map as Map


--- all the data types used in the game are defined here 


-- Coordinates of a point in the map
data Coord = C {cx :: Int ,
                cy :: Int}
                deriving (Show , Eq)


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
    show (ZonId i) = show i

instance Show BatId where
    show (BatId i) = show i

instance Show CitId where
    show (CitId i) = show i

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
        | ZR Forme [Batiment]
        | ZI Forme [Batiment]
        | ZC Forme [Batiment]
        | Admin Forme Batiment
        deriving (Show, Eq)

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
            viZones :: Map ZonId Zone ,
            viCit :: Map CitId Citoyen
            }
            deriving (Show , Eq)
