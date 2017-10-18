{-# LANGUAGE ScopedTypeVariables #-}

module MugiInternalsTest where

import Criptografia
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Data.Maybe

-- TODO : test fromByte
-- TODO : test toByte
-- TODO : test (>>>)
-- TODO : test (<<<)
-- TODO : test getByte
-- TODO : test rearrange
-- TODO : test mul2
-- TODO : test sbox


specs :: Spec
specs
  = describe "rearrange" $ do
      it "rearranges the elements correctly" $
        let xs = [0,1,2] :: [Int]
            is = [1,0,2]
        in rearrange is xs `shouldBe` Just [xs!!1, head xs, xs!!2]

      it "works on empty lists" $
        rearrange [1,0] ([] :: [Int]) `shouldBe` Nothing

      it "works given wrong indices" $
        let xs = [1,2] :: [Int]
            is = [1,0,3]
        in  rearrange is xs `shouldBe` Nothing

      it "works given empty indeces list" $
        let xs = [1,2] :: [Int]
            is = []
        in rearrange is xs `shouldBe` Nothing

test_mugi :: IO TestTree
test_mugi
  = testSpec "MUGI Internal Module" specs
