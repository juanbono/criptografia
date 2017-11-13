{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MugiInternalsTest where

import Criptografia
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Data.ByteString.Arbitrary
import Data.LargeWord
import Data.Word
import Control.Monad
import qualified Data.ByteString as BS

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

    -- Word128 tests
    , testProperty "fromByte128 . toByte128 = id" $
      \x -> (fromByte128 . toByte128) x == x

    , testProperty "length (toByte128 x) = 16" $
      \x -> length (toByte128 x) == 16

    , testProperty "forall x in (toByte128 x). x < 256" $
      let sizeLimit = 256 :: Integer in
      \x -> not $ any (> sizeLimit) (fromIntegral <$> toByte128 x)

    ]

test_loHalf_hiHalf :: TestTree
test_loHalf_hiHalf
  = testGroup "loHalf and hiHalf (Word128)"

  [ testCase "hiHalf (secret key)" $ 0x0001020304050607 @=? hiHalf wordSk
  , testCase "loHalf (secret key)" $ 0x08090a0b0c0d0e0f @=? loHalf wordSk
  ]
  where
    secretKey = [ 0x00, 0x01, 0x02, 0x03
                , 0x04, 0x05, 0x06, 0x07
                , 0x08, 0x09, 0x0a, 0x0b
                , 0x0c, 0x0d, 0x0e, 0x0f ]
    wordSk = fromByte128 secretKey

test_toBS_fromBS :: TestTree
test_toBS_fromBS
  = testGroup "toBS and fromBS tests"

  [ testProperty "fromBS . toBS = id" $
    \(ABS8 b) -> (toBS . fromBS) b == b
  ]

-- this property works only on lists without \NUL characters
test_toWord64List_tests :: TestTree
test_toWord64List_tests
  = testGroup "toWord64List and toWord8List"
  [ testProperty "toWord8List . toWord64List = id" $
    \(x :: [Word8]) ->  (filter (/= 0) . toWord8List . toWord64List) x == filter (/= 0) x
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
    let filterNuls bs = BS.pack (toListOfBytes bs) -- null handling is missing
          where toListOfBytes = toByte . fromBS
                -- nulAscii = 0 :: Integer
    (ABS8 . filterNuls) <$> fastRandBs len

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
   arbitrary = liftM2 LargeKey arbitrary arbitrary
