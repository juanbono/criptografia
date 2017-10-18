module CaesarTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Criptografia
import Control.Lens

test_caesar :: TestTree
test_caesar
  = let k = 1
        ek = caesar^.encrypt $ k
        dk = caesar^.decrypt $ k
    in testGroup "Caesar Cipher"
       [ testProperty "decrypt . encrypt = id" $
         \c -> (dk . ek) c == c
       , testProperty "Only alphabet letters get encrypted" $
         \c -> c > 'Z' ==> ek c == c
       ]
