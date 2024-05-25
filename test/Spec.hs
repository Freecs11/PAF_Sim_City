-- Spec.hs

import qualified AStarTests
import qualified BatimentsTests
import qualified CitoyenTests
import qualified FormeTests
import qualified ZoneTests
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Forme Tests" FormeTests.spec
  describe "Citoyen Tests" CitoyenTests.spec
  describe "Batiment Tests" BatimentsTests.spec
  describe "AStar Tests" AStarTests.spec
  describe "Zone Tests" ZoneTests.spec