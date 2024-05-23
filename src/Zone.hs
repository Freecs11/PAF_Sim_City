module Zone where

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace (trace)

import GameData
import Formes
import GameData

-- get all coords of routes in the city
getRoutesCoords :: Etat -> [Forme]
getRoutesCoords etat@(Etat {ville = ville}) =
  let zones = getZones ville
   in Map.foldrWithKey step [] zones
  where
    step :: ZonId -> Zone -> [Forme] -> [Forme]
    step zonId zone acc =
      case zone of
        Route forme -> forme : acc
        _ -> acc

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
createZone :: Zone -> Int  -> Etat-> Etat
createZone zone cost etat@(Etat {ville = ville})  = 
    case isZoneValid zone etat of
        True -> let zones = getZones ville
                    zonId = ZonId (Map.size zones + 1)
                    updatedZones = Map.insert zonId zone zones
                in etat {ville = ville {viZones = updatedZones} , coins = (coins etat) - cost}  
        False -> etat

-- zones are valid if they are not already in the city
-- and if they are disjoint from the zones already in the city
-- and also if it's a ZR, ZI or ZC zone, it must be adjacent to a route
isZoneValid :: Zone -> Etat -> Bool
isZoneValid zone etat@(Etat {ville = ville}) = 
    let zones = getZones ville
        validity = case zone of
            Eau forme -> not (elem zone zones)
            Route forme -> not (elem zone zones)
            ZR forme _ -> trace ("Disjoint: " ++ show (isDisjointWithMargin forme zones) ++ ", Adjacent: " ++ show (isZoneAdjacentToRoute forme zones))
                (isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones)
            ZI forme _ -> isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones
            ZC forme _ -> isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones
            Admin forme _ -> isDisjointWithMargin forme zones
    in trace ("Zone: " ++ show zone ++ ", Valid: " ++ show validity) validity

isDisjointWithMargin :: Forme -> Map ZonId Zone -> Bool
isDisjointWithMargin forme zones = 
    let intersect = Map.filterWithKey (\zonId zone -> isZoneIntersectWithMargin forme zone) zones
    in Map.null intersect

isZoneIntersectWithMargin :: Forme -> Zone -> Bool
isZoneIntersectWithMargin forme zone = 
    case zone of
        Eau forme' -> collisionWithMargin forme forme'
        Route forme' -> collisionWithMargin forme forme'
        ZR forme' _ -> collisionWithMargin forme forme'
        ZI forme' _ -> collisionWithMargin forme forme'
        ZC forme' _ -> collisionWithMargin forme forme'
        Admin forme' _ -> collisionWithMargin forme forme'

collisionWithMargin :: Forme -> Forme -> Bool
collisionWithMargin f1 f2 = 
    let margin = 1  -- define a margin of error
        (n1, s1, o1, e1) = limites f1
        (n2, s2, o2, e2) = limites f2
    in n1 <= s2 + margin && s1 >= n2 - margin && o1 <= e2 + margin && e1 >= o2 - margin

-- check if a zone is adjacent to a route
isZoneAdjacentToRoute :: Forme -> Map ZonId Zone -> Bool
isZoneAdjacentToRoute forme zones =
  let routes =
        Map.filterWithKey
          ( \zonId zone -> case zone of
              Route _ -> True
              _ -> False
          )
          zones
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