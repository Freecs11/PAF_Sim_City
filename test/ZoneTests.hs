{-# LANGUAGE ScopedTypeVariables #-}

module ZoneTests where

import Test.Hspec
import Test.QuickCheck
import Data.Map (Map)
import qualified Data.Map as Map
import GameData
import Zone
import Formes
instance Arbitrary ZonId where
  arbitrary = ZonId <$> arbitrary

instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Forme where
  arbitrary =
    oneof
      [ HSegment <$> arbitrary <*> (getPositive <$> arbitrary),
        VSegment <$> arbitrary <*> (getPositive <$> arbitrary),
        Rectangle <$> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)
      ]


instance Arbitrary Zone where
  arbitrary =
    oneof
      [ Eau <$> arbitrary,
        Route <$> arbitrary,
        ZR <$> arbitrary <*> arbitrary,
        ZI <$> arbitrary <*> arbitrary,
        ZC <$> arbitrary <*> arbitrary,
        Admin <$> arbitrary <*> arbitrary
      ]


instance Arbitrary Etat where
    arbitrary = pure initialEtat

initialEtat :: Etat
initialEtat = Etat
  { ville = Ville Map.empty Map.empty Map.empty,
    coins = 1000,
    carte = Map.empty,
    currentTime = 0,
    events = Map.empty,
    selection = None,
    world = World { worldOffset = C 0 0 },
    routeDirection = Horizontal,
    selectionStart = Nothing,
    pathCache = Map.empty,
    pathfindingQueue = []
  }
zoneFunctionalityTests :: Spec
zoneFunctionalityTests = do
  it "creates and verifies a zone correctly" $ do
    let zone = Route (Rectangle (C 0 0) 10 10)
        cost = 100
        res = createZone zone cost initialEtat
    isZoneValid zone initialEtat `shouldBe` True
    let etatAfterCreation = createZone zone cost initialEtat
    let zonesAfterCreation = getZones (ville etatAfterCreation)
    Map.size zonesAfterCreation `shouldBe` (Map.size (getZones (ville initialEtat)) + 1)
    coins etatAfterCreation `shouldBe` (coins initialEtat - cost)

  it "validates a zone correctly" $ do
    let zone = Route (Rectangle (C 0 0) 10 10)
    isZoneValid zone initialEtat `shouldBe` True

  it "ensures that an invalid zone is not created" $ do
    let zone = Route (Rectangle (C 0 0) 10 10)
        etatWithZone = createZone zone 100 initialEtat
        zone2 = Route (Rectangle (C 0 0) 10 10)
        res = createZone zone2 100 etatWithZone
    let zonesAfterCreation = getZones (ville res)
    Map.size zonesAfterCreation `shouldBe` (Map.size (getZones (ville etatWithZone)))

propTests :: Spec
propTests = do
  it "ensures invariant on zones" $ property $ \(zone :: Zone) ->
    if isZoneValid zone initialEtat
      then prop_inv_Zones initialEtat
      else True

  it "ensures precondition for createZone" $ property $ \(zone :: Zone) (cost :: Int) (etat :: Etat) ->
    cost >= 0 ==> prop_pre_createZone zone cost etat


spec :: Spec
spec = do
  describe "Zone Tests" $ do
    zoneFunctionalityTests
    propTests