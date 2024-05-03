-- Spec.hs
import Test.Hspec
import qualified FormeTests
import qualified CitoyenTests
import qualified BatimentsTests

main :: IO ()
main = hspec $ do
  describe "Forme Tests" FormeTests.spec
  describe "Citoyen Tests" CitoyenTests.spec
  describe "Batiment Tests" BatimentsTests.spec