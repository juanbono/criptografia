{-# LANGUAGE ScopedTypeVariables #-}

module MugiInternalsTest where

import Criptografia
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec

-- TODO : test (>>>)
-- TODO : test (<<<)
-- TODO : test mul2
-- TODO : test sbox


rearrangeSpecs :: Spec
rearrangeSpecs
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

test_rearrange :: IO TestTree
test_rearrange
  = testSpec "MUGI Internal Module" rearrangeSpecs


test_toByte_fromByte :: TestTree
test_toByte_fromByte
  = testGroup "fromByte and toByte"
    [ testProperty "fromByte . toByte = id" $
      \x -> (fromByte . toByte) x == x
    , testProperty "length (toByte x) = 8" $
      \x -> length (toByte x) == 8
    , testProperty "forall x in (toByte x). x < 256" $
      \x -> not $ any (>256) (fromIntegral <$> toByte x)
    ]
