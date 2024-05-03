{-# LANGUAGE ScopedTypeVariables #-}

module CitoyenTests where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import GameData
import State
import Citoyens

instance Arbitrary CitId where
    arbitrary = CitId <$> arbitrary

instance Arbitrary Coord where
    arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Occupation where
    arbitrary = elements [Dormir, Travailler, FaireCourses, SeDeplacer (C 0 0)]

instance Arbitrary Citoyen where
    arbitrary = oneof [Immigrant <$> arbitrary <*> arbitrary <*> arbitrary,
                       Habitant <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary,
                       Emigrant <$> arbitrary <*> arbitrary]

instance Arbitrary BatId where
    arbitrary = BatId <$> arbitrary




citizenFunctionalityTests :: Spec
citizenFunctionalityTests = do
    it "creates and retrieves a citizen correctly" $ do
        let initialEtat = Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }
            citId = CitId "001"
            citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
            newEtat = createCitoyen citId citoyen initialEtat
        getCitoyenAt (C 1 1) newEtat `shouldBe` Just citId

    it "removes a citizen correctly" $ do
        let initialEtat = Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }
            citId = CitId "001"
            citoyen = Immigrant (C 1 1) (100, 0, 0) Dormir
            stateWithCitizen = createCitoyen citId citoyen initialEtat
            stateWithoutCitizen = removeCitoyen citId stateWithCitizen
        getCitoyenAt (C 1 1) stateWithoutCitizen `shouldBe` Nothing

citizenPropertyTests :: Spec
citizenPropertyTests = do
    it "maintains consistent citizen counts after random adds and removes"
        $ property $ \citoyens ->
            let citIds = map fst citoyens
                etat = foldl (\etat (citId, citoyen) -> createCitoyen citId citoyen etat) 
                             (Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }) 
                             citoyens
                etat' = foldl (\etat citId -> removeCitoyen citId etat) etat citIds
            in all (\citId -> hasCitoyenAt citId etat == hasCitoyenAt citId etat') citIds

citizenInvariantTests :: Spec
citizenInvariantTests = do
    it "maintains the invariant that all citizens in the carte are at the correct coordinates" $ property $
        forAll (listOf arbitrary) $ \citoyens ->
            let citIds = map fst citoyens
                etat = foldl (\etat (citId, citoyen) -> createCitoyen citId citoyen etat) 
                             (Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty }) 
                             citoyens
            in prop_carte_coords_valid_Ville etat

createInitialEtat :: [(CitId, Coord)] -> Etat
createInitialEtat movements = 
    foldl (\etat (citId, coord) -> 
        case getOccupationCitoyen citId etat of
            Just _ -> moveCitizen coord citId etat
            Nothing -> etat) 
    (Etat { ville = Ville Map.empty Map.empty Map.empty, coins = 0, carte = Map.empty, currentTime = 0, events = Map.empty })
    movements


spec :: Spec
spec = do
    citizenFunctionalityTests
    citizenPropertyTests
    citizenInvariantTests
