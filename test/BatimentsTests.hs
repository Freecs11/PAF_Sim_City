{-# LANGUAGE ScopedTypeVariables #-}

module BatimentsTests where

import Test.Hspec
import Test.QuickCheck
import Batiments
import GameData
import State
import Data.Map (Map)
import qualified Data.Map as Map

-- Generating arbitrary instances for testing
instance Arbitrary Coord where
    arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Batiment where
    arbitrary = oneof [
        Cabane <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Atelier <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Epicerie <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Commissariat <$> arbitrary <*> arbitrary ]

instance Arbitrary BatId where
    arbitrary = BatId <$> arbitrary

instance Arbitrary CitId where
    arbitrary = CitId <$> arbitrary

instance Arbitrary Forme where
    arbitrary = oneof [
        HSegment <$> arbitrary <*> arbitrary,
        VSegment <$> arbitrary <*> arbitrary,
        Rectangle <$> arbitrary <*> arbitrary <*> arbitrary ]

-- Tests for creating buildings and verifying their locations
creationAndLocationTests :: Spec
creationAndLocationTests = describe "Creation and Location" $ do
    it "correctly creates and locates buildings" $ property $
        \(batId :: BatId, batiment :: Batiment) -> do
            let ville = Ville Map.empty Map.empty Map.empty
                etat = Etat { ville = ville, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }
                newEtat = createBatiment batId batiment etat
                maybeCoord = getCoordBatiment batId newEtat  
            case maybeCoord of
                Just coord -> isBatimentAt coord batId newEtat -- Check if the building is at the correct location
                Nothing -> False

-- Tests for removing buildings
removalTests :: Spec
removalTests = describe "Removal" $ do
    it "correctly removes buildings" $ property $
        \(batId :: BatId, batiment :: Batiment) -> do
            let ville = Ville Map.empty Map.empty Map.empty
                etat = Etat { ville = ville, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }
                newEtat = createBatiment batId batiment etat
                newEtat' = removeBatiment batId newEtat
                maybeCoord = getCoordBatiment batId newEtat'
            case maybeCoord of
                Just coord -> not $ isBatimentAt coord batId newEtat' -- Check if the building is no longer at the location
                Nothing -> True

-- Tests for verifying the integrity of the state
integrityTests :: Spec
integrityTests = describe "Integrity" $ do
    it "maintains consistent building counts after random adds and removes" $ property $
        \(batiments :: [(BatId, Batiment)]) -> do
            let batIds = map fst batiments
                etat = foldl (\etat (batId, batiment) -> createBatiment batId batiment etat) 
                             (Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }) 
                             batiments
                etat' = foldl (\etat batId -> removeBatiment batId etat) etat batIds
            all (\batId -> hasBatimentAt batId etat == hasBatimentAt batId etat') batIds 



-- Test suite for Batiment functionalities
spec :: Spec
spec = do
    creationAndLocationTests
    removalTests
    integrityTests
