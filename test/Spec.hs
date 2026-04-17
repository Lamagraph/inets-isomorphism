import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Graph.Inductive
import Lib

main :: IO ()
main = hspec $ do
  describe "Invariants" $ do
    it "should return True for identical graphs" $ do
      let g = mkGraph [(1, "A")] [(1, 1, "L")] :: Gr String String
      checkInvariants g g `shouldBe` True

  describe "Isomorphism" $ do
    it "should be True for same structure with different IDs" $ do
      let g1 = mkGraph [(1, "a"), (2, "b")] [(1, 2, "e")] :: Gr String String
          g2 = mkGraph [(10, "a"), (20, "b")] [(10, 20, "e")] :: Gr String String
      isIsomorphic g1 g2 `shouldBe` True

    it "should be False for different edge counts" $ do
      let g1 = mkGraph [(1, "a"), (2, "b")] [(1, 2, "e")] :: Gr String String
          g2 = mkGraph [(1, "a"), (2, "b")] [] :: Gr String String
      isIsomorphic g1 g2 `shouldBe` False

    prop "reflexivity" $ \n ->
      let g = mkGraph [(1, n :: Int)] [] :: Gr Int Int
      in isIsomorphic g g
