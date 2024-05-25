module Zone where

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace (trace)

import Formes
import GameData

-- get tous les routes de la ville
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

-- vérifie si une zone est à une coordonnée
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

-- créé une zone si elle est valide 
createZone :: Zone -> Int  -> Etat-> Etat
createZone zone cost etat@(Etat {ville = ville})  = 
    case isZoneValid zone etat of
        True -> let zones = getZones ville
                    zonId = ZonId (Map.size zones + 1)
                    updatedZones = Map.insert zonId zone zones
                in etat {ville = ville {viZones = updatedZones} , coins = (coins etat) - cost}  
        False -> etat

-- les zones sont valides si elles ne sont pas déjà dans la ville
-- et si elles sont disjointes des zones déjà dans la ville
-- et aussi si c'est une zone ZR, ZI ou ZC, elle doit être adjacente à une route
isZoneValid :: Zone -> Etat -> Bool
isZoneValid zone etat@(Etat {ville = ville}) = 
    let zones = getZones ville
        validity = case zone of
            Eau forme -> not (elem zone zones)
            Route forme -> not (elem zone zones) && isDisjointWithMargin forme zones
            ZR forme _ -> trace ("Disjoint: " ++ show (isDisjointWithMargin forme zones) ++ ", Adjacent: " ++ show (isZoneAdjacentToRoute forme zones))
                (isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones)
            ZI forme _ -> isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones
            ZC forme _ -> isDisjointWithMargin forme zones && isZoneAdjacentToRoute forme zones
            Admin forme _ -> isDisjointWithMargin forme zones
    in trace ("Zone: " ++ show zone ++ ", Valid: " ++ show validity) validity

-- vérifie si une forme est disjointe des autres formes avec une marge
isDisjointWithMargin :: Forme -> Map ZonId Zone -> Bool
isDisjointWithMargin forme zones = 
    let intersect = Map.filterWithKey (\zonId zone -> isZoneIntersectWithMargin forme zone) zones
    in Map.null intersect

-- vérifie si une zone est en collision avec une autre zone avec une marge ( marge est 1, dans collision)
isZoneIntersectWithMargin :: Forme -> Zone -> Bool
isZoneIntersectWithMargin forme zone = 
    case zone of
        Eau forme' -> collision forme forme'
        Route forme' -> collision forme forme'
        ZR forme' _ -> collision forme forme'
        ZI forme' _ -> collision forme forme'
        ZC forme' _ -> collision forme forme'
        Admin forme' _ -> collision forme forme'


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
   in any (\zone -> adjacentes forme (zoneForme zone)) routes


zoneForme :: Zone -> Forme
zoneForme (ZR forme _) = forme
zoneForme (ZI forme _) = forme
zoneForme (ZC forme _) = forme
zoneForme (Eau forme) = forme
zoneForme (Route forme) = forme
zoneForme (Admin forme _) = forme

-- coord of a zone 
zoneCoord :: Zone -> Coord
zoneCoord (ZR forme _) = getFormeCoord forme
zoneCoord (ZI forme _) = getFormeCoord forme
zoneCoord (ZC forme _) = getFormeCoord forme
zoneCoord (Eau forme) = getFormeCoord forme
zoneCoord (Route forme) = getFormeCoord forme
zoneCoord (Admin forme _) = getFormeCoord forme

-- invarinat on zones 
prop_inv_Zones :: Etat -> Bool
prop_inv_Zones etat@(Etat {ville = ville}) =
  let zones = getZones ville
   in Map.foldrWithKey step True zones
  where
    step :: ZonId -> Zone -> Bool -> Bool
    step zonId zone acc = acc && isZoneValid zone etat

prop_pre_createZone :: Zone -> Int -> Etat -> Bool
prop_pre_createZone zone cost etat = cost >= 0

prop_post_createZone :: Zone -> Int -> Etat -> Etat -> Bool
prop_post_createZone zone cost etat res =
  let zones = getZones (ville res)
   in case isZoneValid zone res of
        True -> Map.size zones == Map.size (getZones (ville etat)) + 1
        False -> Map.size zones == Map.size (getZones (ville etat))
