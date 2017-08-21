{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{- The Affine Cipher (Block Cipher)
 * Plaintext = Zm
 * Key space = All the pairs (a, b) in Zm^2 for which m is prime to a.
 * Encryption function Ek for the key k = (a, b):
   Ek : Char -> Char, \x. ax + b mod m

 * Decryption function Dk for the key k = (a', b):
   Dk : Char -> Char, \x. a' * (x - b) mod m
-}

module Criptografia.Affine
  (affine) where

import Data.Modular
import qualified Data.Vector.Unboxed as V
import Criptografia.Cipher

affine :: Cipher (V.Vector Char) Key Char
affine = MkCipher
  { _alphabet = affineAlphabet
  , _encrypt  = affineEncrypt
  , _decrypt  = affineDecrypt
  }

type Key = (Int/26, Int/26)

affineAlphabet :: V.Vector Char
affineAlphabet = V.enumFromTo 'A' 'Z'

affineEncrypt :: Key -> Char -> Char
affineEncrypt (a, b) c
  = case V.elemIndex c affineAlphabet of
      Just x  -> affineAlphabet V.! unMod (a * toMod x + b)
      Nothing -> c

affineDecrypt :: Key -> Char -> Char
affineDecrypt (a, b) c
  = case V.elemIndex c affineAlphabet of
      Just x  -> affineAlphabet V.! unMod (inv a * (toMod x - b))
      Nothing -> c
