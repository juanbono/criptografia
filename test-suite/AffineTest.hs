module AffineTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Criptografia
import Lens.Micro.Platform

test_affine :: TestTree
test_affine
  = let k = (3,6)
        ek = affine^.encrypt $ k
        dk = affine^.decrypt $ k
    in testGroup "Affine Cipher"
       [ testProperty "decrypt . encrypt = id" $
         \c -> (dk . ek) c == c
       , testProperty "Only alphabet letters get encrypted" $
         \c -> c > 'Z' ==> ek c == c
       ]
