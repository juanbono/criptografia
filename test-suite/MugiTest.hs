
module MugiTest where

import Criptografia
import Test.Tasty
import Test.Tasty.HUnit
import Data.Word
import Data.LargeWord
import qualified Data.Vector.Unboxed as V
import Control.Lens ((^.))

main :: IO ()
main = defaultMain test_init

{-
key[16] =
{0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f}
iv[16] =
{0xf0 0xe0 0xd0 0xc0 0xb0 0xa0 0x90 0x80 0x70 0x60 0x50 0x40 0x30 0x20 0x10 0x00}
-}

test_init :: TestTree
test_init
  = testGroup "Unit tests"
  [ initUnitTests
  ]

key :: [Word8]
key = [ 0x00, 0x01, 0x02, 0x03
      , 0x04, 0x05, 0x06, 0x07
      , 0x08, 0x09, 0x0a, 0x0b
      , 0x0c, 0x0d, 0x0e, 0x0f ]

iv :: [Word8]
iv = [ 0xf0, 0xe0, 0xd0, 0xc0
     , 0xb0, 0xa0, 0x90, 0x80
     , 0x70, 0x60, 0x50, 0x40
     , 0x30, 0x20, 0x10, 0x00 ]

secret :: Word128
secret = fromByte128 key

initVector :: Word128
initVector = fromByte128 iv

stateAfterKeyInput :: IState
stateAfterKeyInput = IState
      { _stateA = V.fromList [a0, a1, a2]
      , _stateB = emptyBuffer }
  where
    a0 = 0x01020304050607
    a1 = 0x08090a0b0c0d0e0f
    a2 = 0x7498f5f1e727d094

stateAfterMix :: IState
stateAfterMix = IState
  { _stateA = V.fromList [0x7dea261cb61d4fea, 0xeafb528479bb687d, 0xeb8189612089ff0b]
  , _stateB = V.fromList bs
  } where
  bs = [ 0x7dea261cb61d4fea, 0xbfe2485ac2696cc7, 0xc905d08f50fa71db, 0xfd5755df9cc0ceb9
       , 0x5cc4835080bc5321, 0xdfbbb88c02c9c80a, 0x591a6857e3112cee, 0x20ead0479e63cdc3
       , 0x2d13c00221057d8d, 0xb36b4d944f5d04cb, 0x738177859f3210f6, 0xc08ee4dcb2d08591
       , 0x9c0c2097edb20067, 0x09671cfbcfaa95fb, 0x9724d9144c5d8926, 0x08090a0b0c0d0e0f ]

stateAfterIVInput :: IState
stateAfterIVInput = IState
  { _stateA = V.fromList [0x8d0af6dc06bddf6a, 0x9a9b02c4499b787d, 0xf100cffe031d365b]
  , _stateB = V.fromList bs
  } where
  bs = [ 0x7dea261cb61d4fea, 0xbfe2485ac2696cc7, 0xc905d08f50fa71db, 0xfd5755df9cc0ceb9
       , 0x5cc4835080bc5321, 0xdfbbb88c02c9c80a, 0x591a6857e3112cee, 0x20ead0479e63cdc3
       , 0x2d13c00221057d8d, 0xb36b4d944f5d04cb, 0x738177859f3210f6, 0xc08ee4dcb2d08591
       , 0x9c0c2097edb20067, 0x09671cfbcfaa95fb, 0x9724d9144c5d8926, 0x08090a0b0c0d0e0f ]

stateAfterSecondMixing :: IState
stateAfterSecondMixing = IState
  { _stateA = V.fromList [0x4e466dffcb92db48, 0xf5eb67b928359d8b, 0x5d3c31a0af9cd78f]
  , _stateB = V.fromList bs
  } where
  bs = [ 0x7dea261cb61d4fea, 0xbfe2485ac2696cc7, 0xc905d08f50fa71db, 0xfd5755df9cc0ceb9
       , 0x5cc4835080bc5321, 0xdfbbb88c02c9c80a, 0x591a6857e3112cee, 0x20ead0479e63cdc3
       , 0x2d13c00221057d8d, 0xb36b4d944f5d04cb, 0x738177859f3210f6, 0xc08ee4dcb2d08591
       , 0x9c0c2097edb20067, 0x09671cfbcfaa95fb, 0x9724d9144c5d8926, 0x08090a0b0c0d0e0f ]

stateAfterInit :: IState
stateAfterInit = IState
  { _stateA = V.fromList [0x0ce5a4d1a0cbc0f7, 0x316993816117e50f, 0xbc62430614b79b71]
  , _stateB = V.fromList bs
  } where
  bs = [ 0xd25c6643a9dabd67, 0xe893c5b5a5b2ff2b, 0xce840df556562dc6, 0x4210def4ccf1b145
       , 0x5eda7c5b0dbf1554, 0xd3e8a809b214218a, 0xd42bcb0bb4811480, 0x76d9c281df20192d
       , 0x3dc6c6bc876beb72, 0x39d84df58f8840e2, 0xcd7fe2794367de6c, 0x680920245819a4f5
       , 0xf5e9e609dd8e3cc3, 0x9cf94157cf512603, 0x871323e1d70caa2b, 0x0b6bb4c0466c7aba ]

output :: [Word64]
output = [ 0xbc62430614b79b71, 0x71a66681c35542de, 0x7aba5b4fb80e82d7, 0x0b96982890b6e143
         , 0x4930b5d033157f46, 0xb96ed8499a282645, 0xdbeb1ef16d329b15, 0x34a9192c4ddcf34e ]

initUnitTests :: TestTree
initUnitTests
  = testGroup "init function works fine"
  [ testCase "firstStep" $ stateAfterKeyInput @=? v1
  , testCase "mixing with rho - 1" $ stateAfterMix @=? v2
  , testCase "secondStep: IV input" $ stateAfterIVInput @=? v3
  , testCase "mixing with rho - 2" $ stateAfterSecondMixing @=? v4
  , testCase "thirdStep" $ stateAfterInit @=? v5
  , testCase "output" $ output @=? v6
  ] where
  v1 = firstStep secret
  v2 = mixing v1
  v3 = ivInput v2 initVector
  v4 = IState (mixing v3^.stateA) (v3^.stateB)
  v5 = thirdStep v4
  v6 = take 8 $ randomStream v5
