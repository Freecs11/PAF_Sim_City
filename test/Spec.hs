-- Spec.hs
import Test.Hspec
import qualified FormeTests

main :: IO ()
main = hspec $ do
  describe "Forme Tests" FormeTests.spec
