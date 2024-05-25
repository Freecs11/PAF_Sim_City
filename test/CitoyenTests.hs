{-# LANGUAGE ScopedTypeVariables #-}

module CitoyenTests where

import Citoyens
import qualified Data.Map as Map
import GameData
import State
import Test.Hspec
import Test.QuickCheck

instance Arbitrary CitId where
  arbitrary = CitId <$> arbitrary

instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Occupation where
  arbitrary = elements [Dormir, Travailler, FaireCourses, SeDeplacer (C 0 0)]

instance Arbitrary Citoyen where
  arbitrary =
    oneof
      [ Immigrant <$> arbitrary <*> arbitrary <*> arbitrary,
        Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
        Emigrant <$> arbitrary <*> arbitrary
      ]

instance Arbitrary BatId where
  arbitrary = BatId <$> arbitrary

citizenFunctionalityTests :: Spec
citizenFunctionalityTests = do
  it "creates and retrieves a citizen correctly" $ do
    let initialEtat = Etat {ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty , selection = None
    , world = World { worldOffset = C 0 0} , routeDirection = Horizontal, selectionStart = Nothing, pathCache = Map.empty
        , pathfindingQueue = []}
        citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
        (newEtat, citId) = createCitoyen citoyen initialEtat
    getCitoyenAt (C 1 1) newEtat `shouldBe` Just citId
    prop_inv_Citoyen newEtat `shouldBe` True

  it "removes a citizen correctly" $ do
    let initialEtat = Etat {ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty , selection = None
    , world = World { worldOffset = C 0 0} , routeDirection = Horizontal, selectionStart = Nothing, pathCache = Map.empty
        , pathfindingQueue = []}
        citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
        (stateWithCitizen, citId) = createCitoyen citoyen initialEtat
        stateWithoutCitizen = removeCitoyenFromVille citId stateWithCitizen
    getCitoyenAt (C 1 1) stateWithoutCitizen `shouldBe` Nothing
    prop_inv_Citoyen stateWithoutCitizen `shouldBe` True

-- Additional property-based tests
propertyBasedTests :: Spec
propertyBasedTests = describe "Property-based tests" $ do
  it "maintains valid state after random operations" $ property $
    \(citizens :: [Citoyen]) -> do
      let (etat, citIds) =
            foldl
              (\(etat, citIds) citizen -> let (etat', citId) = createCitoyen citizen etat in (etat', citId : citIds))
              (Etat
                { ville = Ville Map.empty Map.empty Map.empty,
                  coins = 0,
                  carte = Map.empty,
                  currentTime = 0,
                  events = Map.empty,
                  selection = None,
                  world = World { worldOffset = C 0 0 },
                  routeDirection = Horizontal,
                  selectionStart = Nothing,
                  pathCache = Map.empty,
                  pathfindingQueue = []
                },
                []
              )
              citizens
          etat' = foldl (\etat citId -> removeCitoyenFromVille citId etat) etat citIds
      prop_carte_coords_valid_Ville etat `shouldBe` True
      prop_inv_Citoyen etat `shouldBe` True
      prop_carte_coords_valid_Ville etat' `shouldBe` True
      prop_inv_Citoyen etat' `shouldBe` True

-- Test suite for Citoyen functionalities
spec :: Spec
spec = do
  citizenFunctionalityTests
  propertyBasedTests
