module Zone where

import Data.Map (Map)
import qualified Data.Map as Map


import GameData
import Formes



-- check if a zone is at a coordinate
isZoneAt :: Coord -> ZonId -> Etat -> Bool
isZoneAt coord zonId (Etat {ville = ville}) = 
    let zones = getZones ville
    in case Map.lookup zonId zones of
        Just zone -> case zone of
            Eau forme -> coord == getFormeCoord forme
            Route forme -> coord == getFormeCoord forme
            ZR forme _ -> coord == getFormeCoord forme
            ZI forme _ -> coord == getFormeCoord forme
            ZC forme _ -> coord == getFormeCoord forme
            Admin forme _ -> coord == getFormeCoord forme
        Nothing -> False


-- create a zone if it's valid
createZone :: Zone -> Etat -> Etat
createZone zone etat@(Etat {ville = ville}) = 
    case isZoneValid zone etat of
        True -> let zones = getZones ville
                    zonId = ZonId (Map.size zones)
                    updatedZones = Map.insert zonId zone zones
                in etat {ville = ville {viZones = updatedZones}}
        False -> etat

-- zones are valid if they are not already in the city 
-- and if they are disjoint from the zones already in the city
-- and also if it's a ZR, ZI or ZC zone, it must be adjacent to a route 
isZoneValid :: Zone -> Etat -> Bool
isZoneValid zone etat@(Etat {ville = ville}) = 
    let zones = getZones ville
    in case zone of
        Eau forme -> not (elem zone zones)
        Route forme -> not (elem zone zones)
        ZR forme _ -> not (elem zone zones) && isZoneDisjoint forme zones && isZoneAdjacentToRoute forme zones
        ZI forme _ -> not (elem zone zones) && isZoneDisjoint forme zones && isZoneAdjacentToRoute forme zones
        ZC forme _ -> not (elem zone zones) && isZoneDisjoint forme zones && isZoneAdjacentToRoute forme zones
        Admin forme _ -> not (elem zone zones) && isZoneDisjoint forme zones

-- check if a zone is disjoint from the other zones
isZoneDisjoint :: Forme -> Map ZonId Zone -> Bool
isZoneDisjoint forme zones = 
    let intersect = Map.filterWithKey (\zonId zone -> isZoneIntersect forme zone) zones
    in Map.null intersect


-- check if two zones intersect
isZoneIntersect :: Forme -> Zone -> Bool
isZoneIntersect forme zone = 
    case zone of
        Eau forme' -> adjacentes forme forme'
        Route forme' -> adjacentes forme forme'
        ZR forme' _ -> adjacentes forme forme'
        ZI forme' _ -> adjacentes forme forme'
        ZC forme' _ -> adjacentes forme forme'
        Admin forme' _ -> adjacentes forme forme'

-- check if a zone is adjacent to a route
isZoneAdjacentToRoute :: Forme -> Map ZonId Zone -> Bool
isZoneAdjacentToRoute forme zones = 
    let routes = Map.filterWithKey (\zonId zone -> case zone of
            Route _ -> True
            _ -> False) zones
    in any (\zone -> adjacentes forme (getForme zone)) routes

    
getForme :: Zone -> Forme
getForme zone = 
    case zone of
        Eau forme -> forme
        Route forme -> forme
        ZR forme _ -> forme
        ZI forme _ -> forme
        ZC forme _ -> forme
        Admin forme _ -> forme


-- invarinat on zones 
prop_inv_Zones :: Etat -> Bool
prop_inv_Zones etat@(Etat {ville = ville}) = 
    let zones = getZones ville
    in Map.foldrWithKey step True zones
    where
        step :: ZonId -> Zone -> Bool -> Bool
        step zonId zone acc = acc && isZoneValid zone etat