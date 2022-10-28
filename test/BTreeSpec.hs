module BTreeSpec where

import Prelude hiding (sum, max, min)
import Test.Hspec
import BTree

spec :: Spec
spec =
  describe "BTree" $ do
    it "size Leaf" $ do
      size (Leaf 1) `shouldBe` 1
    it "size Branch" $ do
      size (Branch 1 (Leaf 1) (Leaf 2)) `shouldBe` 3
    it "sum Leaf" $ do
      sum (Leaf 1) `shouldBe` 1
    it "size Branch" $ do
      sum (Branch 1 (Leaf 1) (Leaf 2)) `shouldBe` 4
    it "avg Leaf" $ do
      avg (Leaf 1) `shouldBe` 1
    it "avg Branch" $ do
      avg (Branch 1 (Leaf 1) (Leaf 1)) `shouldBe` 1.0
    it "min Leaf" $ do
      min (Leaf 1) `shouldBe` 1
    it "min Branch" $ do
      min (Branch 2 (Leaf 1) (Leaf 3)) `shouldBe` 1
    it "max Leaf" $ do
      max (Leaf 1) `shouldBe` 1
    it "max Branch" $ do
      max (Branch 2 (Leaf 1) (Leaf 3)) `shouldBe` 3
    it "find Leaf" $ do
      let n = Leaf 1
      find n 1 `shouldBe` Just n
    it "find Branch" $ do
      let n1 = Leaf 3
      let n2 = Branch 2 (Leaf 1) n1
      find n2 3 `shouldBe` Just n1
    it "fmap Leaf" $ do
      let n = Leaf 1
      fmap (*2) n `shouldBe` Leaf 2
    it "fmap Branch" $ do
      let n2 = Branch 2 (Leaf 1) (Leaf 3)
      fmap (*2) n2 `shouldBe` Branch 4 (Leaf 2) (Leaf 6)
    it "bind Leaf" $ do
      (Leaf 1 >>= (\x -> Leaf (x * 2))) `shouldBe` Leaf 2
    it "bind Branch" $ do
      (Branch 2 (Leaf 1) (Leaf 3) >>= (\x -> Leaf (x * 2))) `shouldBe` Leaf 4
