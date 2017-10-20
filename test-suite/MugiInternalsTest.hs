{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module MugiInternalsTest where

import Criptografia
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Data.ByteString.Arbitrary
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS

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
  = testGroup "fromByte and toByte tests"

    [ testProperty "fromByte . toByte = id" $
      \x -> (fromByte . toByte) x == x

    , testProperty "length (toByte x) = 8" $
      \x -> length (toByte x) == 8

    , testProperty "forall x in (toByte x). x < 256" $
      let sizeLimit = 256 :: Integer in
      \x -> not $ any (> sizeLimit) (fromIntegral <$> toByte x)
    ]

test_toBS_fromBS :: TestTree
test_toBS_fromBS
  = testGroup "toBS and fromBS tests"

  [ testProperty "fromBS . toBS = id" $
    \(ABS8 b) -> (toBS . fromBS . B.fromStrict) b == B.fromStrict b
  ]


-- | A ByteString wrapper so we can implement Arbitrary for ByteString. This
-- will currently generate random ByteStrings of length 0 to 8B.
newtype ArbByteString8 = ABS8 { fromABS8 :: BS.ByteString }
  deriving (Eq, Ord, Read, Show)

-- TODO: Test this instance
-- generates an arbitrary 64-bit ByteString that doesn't contain null characters
instance Arbitrary ArbByteString8 where
  arbitrary = do
    len <- choose (0, 8)
    let filterNuls bs = packAndtoStrict $ filter (/= nulAscii) (toListOfBytes bs)
          where packAndtoStrict = B.toStrict . B.pack
                toListOfBytes = toByte . fromBS . B.fromStrict
                nulAscii = 0
    (ABS8 . filterNuls) <$> fastRandBs len
