{-# LANGUAGE ScopedTypeVariables #-}

module FormeTests where

import Formes
import GameData
import Test.Hspec
import Test.QuickCheck

-- Define Arbitrary instances for the types Coord and Forme
instance Arbitrary Coord where
  arbitrary = C <$> arbitrary <*> arbitrary

instance Arbitrary Forme where
  arbitrary =
    oneof
      [ HSegment <$> arbitrary <*> (getPositive <$> arbitrary),
        VSegment <$> arbitrary <*> (getPositive <$> arbitrary),
        Rectangle <$> arbitrary <*> (getPositive <$> arbitrary) <*> (getPositive <$> arbitrary)
      ]

-- Test suite for limites function
limitesSpec :: Spec
limitesSpec = do
  describe "Formes.limites" $ do
    it "obeys the precondition that all dimensions are positive" $
      property $
        \(forme :: Forme) -> prop_limites_precondition forme ==> True

    it "calculates correct limits for an HSegment" $
      property $ \(x :: Int, y :: Int, Positive l) ->
        let forme = HSegment (C x y) l
         in prop_limites_postcondition forme

    it "calculates correct limits for a VSegment" $
      property $ \(x :: Int, y :: Int, Positive l) ->
        let forme = VSegment (C x y) l
         in prop_limites_postcondition forme

    it "calculates correct limits for a Rectangle" $
      property $ \(x :: Int, y :: Int, Positive w, Positive h) ->
        let forme = Rectangle (C x y) w h
         in prop_limites_postcondition forme

-- Test suite for appartient function
appartientSpec :: Spec
appartientSpec = do
  describe "Formes.appartient" $ do
    it "correctly identifies if a coord is within an HSegment" $
      property $ \(C x y, Positive l) ->
        let forme = HSegment (C x y) l
         in all (\cx -> appartient (C cx y) forme) [x .. x + l - 1]

    it "correctly identifies if a coord is within a VSegment" $
      property $ \(C x y, Positive l) ->
        let forme = VSegment (C x y) l
         in all (\cy -> appartient (C x cy) forme) [y .. y + l - 1]

    it "correctly identifies if a coord is within a Rectangle" $
      property $ \(C x y, Positive w, Positive h) ->
        let forme = Rectangle (C x y) w h
         in all (\(cx, cy) -> appartient (C cx cy) forme) [(cx, cy) | cx <- [x .. x + w - 1], cy <- [y .. y + h - 1]]

    it "ensures coordinates just outside of HSegment boundaries are not included" $
      property $ \(C x y, Positive l) ->
        let forme = HSegment (C x y) l
         in not (appartient (C (x - 1) y) forme) && not (appartient (C (x + l) y) forme)

    it "ensures coordinates just outside of VSegment boundaries are not included" $
      property $ \(C x y, Positive l) ->
        let forme = VSegment (C x y) l
         in not (appartient (C x (y - 1)) forme) && not (appartient (C x (y + l)) forme)

    it "ensures coordinates just outside of Rectangle boundaries are not included" $
      property $ \(C x y, Positive w, Positive h) ->
        let forme = Rectangle (C x y) w h
         in not (appartient (C (x - 1) y) forme)
              && not (appartient (C (x + w) y) forme)
              && not (appartient (C x (y - 1)) forme)
              && not (appartient (C x (y + h)) forme)

    it "obeys the precondition that all dimensions are positive" $
      property $
        \(coord :: Coord, forme :: Forme) -> prop_appartient_precondition coord forme ==> True

    it "obeys the invariant that a coord is either in a Forme or adjacent to it" $
      property $
        \(coord :: Coord, forme :: Forme) -> prop_appartient_invariant coord forme

    it "obeys the postcondition that appartient returns the correct value" $
      property $
        \(coord :: Coord, forme :: Forme) -> prop_appartient_postcondition coord forme

-- Test suite for adjacent function
adjacentSpec :: Spec
adjacentSpec = do
  describe "Formes.adjacent" $ do
    it "correctly identifies if a coord is adjacent to an HSegment" $
      property $ \(C x y, Positive l) ->
        let forme = HSegment (C x y) l
            margin = 10
         in all (\cx -> adjacent (C cx y) forme) [x - margin, x + l + margin]

    it "correctly identifies if a coord is adjacent to a VSegment" $
      property $ \(C x y, Positive l) ->
        let forme = VSegment (C x y) l
            margin = 10
         in all (\cy -> adjacent (C x cy) forme) [y - margin, y + l + margin]

    it "correctly identifies if a coord is adjacent to a Rectangle" $
      property $ \(C x y, Positive w, Positive h) ->
        let forme = Rectangle (C x y) w h
            margin = 10
            adjacents =
              [(x - margin, y + dy) | dy <- [0 .. h - 1]]
                ++ [(x + w + margin, y + dy) | dy <- [0 .. h - 1]]
                ++ [(x + dx, y - margin) | dx <- [0 .. w - 1]]
                ++ [(x + dx, y + h + margin) | dx <- [0 .. w - 1]]
         in all (\(cx, cy) -> adjacent (C cx cy) forme) adjacents


    it "ensures coordinates just outside of HSegment boundaries are not considered adjacent" $
      property $ \(C x y, Positive l) ->
        let forme = HSegment (C x y) l
            margin = 10
         in not (adjacent (C (x - margin - 1) y) forme) && not (adjacent (C (x + l + margin + 1) y) forme)

    it "ensures coordinates just outside of VSegment boundaries are not considered adjacent" $
      property $ \(C x y, Positive l) ->
        let forme = VSegment (C x y) l
            margin = 10
         in not (adjacent (C x (y - margin - 1)) forme) && not (adjacent (C x (y + l + margin + 1)) forme)

    it "ensures coordinates just outside of Rectangle boundaries are not considered adjacent" $
      property $ \(C x y, Positive w, Positive h) ->
        let forme = Rectangle (C x y) w h
            margin = 10
         in not (adjacent (C (x - margin - 1) y) forme)
              && not (adjacent (C (x + w + margin + 1) y) forme)
              && not (adjacent (C x (y - margin - 1)) forme)
              && not (adjacent (C x (y + h + margin + 1)) forme)

    it "obeys the precondition that all dimensions are positive" $
      property $
        \(coord :: Coord, forme :: Forme) -> prop_adjacent_precondition coord forme ==> True

    it "obeys the invariant that a coord is either in a Forme or adjacent to it" $
      property $
        \(coord :: Coord, forme :: Forme) -> prop_adjacent_invariant coord forme

-- Test suite for collision function
collisionSpec :: Spec
collisionSpec = do
  describe "Formes.collision" $ do
    it "correctly identifies if two Formes collide" $
      property $ \(forme1 :: Forme, forme2 :: Forme) ->
        prop_collision_postcondition forme1 forme2
          

    it "obeys the precondition that all dimensions are positive" $
      property $ \(forme1 :: Forme, forme2 :: Forme) ->
        prop_collision_precondition forme1 forme2 ==> True

    it "obeys the postcondition that collision returns the correct value" $
      property $ \(forme1 :: Forme, forme2 :: Forme) ->
        prop_collision_postcondition forme1 forme2

    it "obeys the invariant that collision is symmetric" $
      property $ \(forme1 :: Forme, forme2 :: Forme) ->
        prop_collision_invariant forme1 forme2

-- Test suite for adjacentes function
adjacentesSpec :: Spec
adjacentesSpec = do
  describe "Formes.adjacentes" $ do
    it "identifies two forms as adjacent when they meet edge to edge horizontally" $ do
      let form1 = HSegment (C 0 0) 5
          form2 = HSegment (C 5 0) 5
      adjacentes form1 form2 `shouldBe` True

    it "identifies two forms as adjacent when they meet edge to edge vertically" $ do
      let form1 = VSegment (C 0 0) 5
          form2 = VSegment (C 0 5) 5
      adjacentes form1 form2 `shouldBe` True

    it "does not identify forms as adjacent if they only touch at a corner" $ do
      let form1 = Rectangle (C 0 0) 5 5
          -- we took +15 to ensure that we take the margin into account
          form2 = Rectangle (C 15 15) 5 5 
      adjacentes form1 form2 `shouldBe` False

    it "does not identify forms as adjacent if there is a gap" $ do
      let form1 = Rectangle (C 0 0) 5 5
          form2 = Rectangle (C 17 0) 5 5
      adjacentes form1 form2 `shouldBe` False

-- All tests combined
spec :: Spec
spec = do
  limitesSpec
  appartientSpec
  adjacentSpec
  collisionSpec
  adjacentesSpec
