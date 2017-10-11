{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Criptografia.AffineLinear where

import Numeric.LinearAlgebra
import Data.Modular
import qualified Data.Vector as V

import Criptografia.Cipher

aM = matrix 2 [1,2,3,4]
