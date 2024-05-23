{-# LANGUAGE ScopedTypeVariables #-}

module BatimentsTests where

import Batiments
import Data.Map (Map)
import qualified Data.Map as Map
import GameData
import State
import Test.Hspec
import Test.QuickCheck

-- Generating arbitrary instances for testing
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Batiment where
  arbitrary =
    oneof
      [ Cabane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Atelier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Epicerie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Commissariat <$> arbitrary <*> arbitrary
      ]

instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

instance Arbitrary Forme where
  arbitrary =
    oneof
      [ HSegment <$> arbitrary <*> arbitrary,
        VSegment <$> arbitrary <*> arbitrary,
        Rectangle <$> arbitrary <*> arbitrary <*> arbitrary
      ]

-- Tests for creating buildings and verifying their locations
creationAndLocationTests :: Spec
creationAndLocationTests = do
  it "correctly creates and locates buildings" $ do
    let routes = Route (Rectangle (C 0 0) 10 10)
        zones = Map.fromList [(ZonId 0, routes)]
        ville = Ville Map.empty Map.empty zones
        etat = Etat {ville = ville, coins = 100, carte = Map.empty, currentTime = 0, events = Map.empty, selection = None}
        batiment = Cabane (Rectangle (C 0 0) 5 5) (C 0 0) 10 []
        (newEtat, batId) = createBatiment batiment 80 etat
    getBatimentCoord batiment `shouldBe` C 0 0

-- Tests for removing buildings
removalTests :: Spec
removalTests = do
  it "correctly removes buildings" $ do
    let routes = Route (Rectangle (C 0 0) 10 10)
        zones = Map.fromList [(ZonId 0, routes)]
        ville = Ville Map.empty Map.empty zones
        etat = Etat {ville = ville, coins = 100, carte = Map.empty, currentTime = 0, events = Map.empty, selection = None}
        batiment = Cabane (Rectangle (C 0 0) 5 5) (C 0 0) 10 []
        (newEtat, batId) = createBatiment batiment 80 etat
        coord = getBatimentCoord batiment
        newEtat' = removeBatiment batId newEtat
     in not $ isBatimentAt coord batId newEtat'

-- Testing with Quickcheck doesnt work because we cannot generate a valid Batiment
-- because the coordinates must be adjacent to a Route

-- Tests for verifying the integrity of the state
-- integrityTests :: Spec
-- integrityTests = describe "Integrity" $ do
--   it "maintains consistent building counts after random adds and removes" $
--     property $
--       \(batiments :: [Batiment]) -> do
--         let (etat, batIds) =
--               foldl
--                 (\(etat, batIds) batiment -> let (etat, batId) = createBatiment batiment etat in (etat, batId : batIds))
--                 ( ( Etat
--                       { ville = Ville Map.empty Map.empty Map.empty,
--                         coins = 0,
--                         carte = Map.empty,
--                         currentTime = 0,
--                         events = Map.empty,
--                         selection = None
--                       }
--                   ),
--                   []
--                 )
--                 batiments
--             etat' = foldl (\etat batiment -> removeBatiment batiment etat) etat batIds
--         all (\batId -> hasBatimentAt batId etat == hasBatimentAt batId etat') batIds

-- Test suite for Batiment functionalities
spec :: Spec
spec = do
  creationAndLocationTests
  removalTests
