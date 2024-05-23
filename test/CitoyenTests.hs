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
    let initialEtat = Etat {ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty}
        citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
        (newEtat, citId) = createCitoyen citoyen initialEtat
    getCitoyenAt (C 1 1) newEtat `shouldBe` Just citId

  it "removes a citizen correctly" $ do
    let initialEtat = Etat {ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty}
        citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
        (stateWithCitizen, citId) = createCitoyen citoyen initialEtat
        stateWithoutCitizen = removeCitoyenFromVille citId stateWithCitizen
    getCitoyenAt (C 1 1) stateWithoutCitizen `shouldBe` Nothing

spec :: Spec
spec = do
  citizenFunctionalityTests
