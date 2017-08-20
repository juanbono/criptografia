{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{- The Affine Cipher (Block Cipher)
 * Plaintext = Zm
 * Key space = All the pairs (a, b) in Zm^2 for which m is prime to a.
 * Encryption function Ek for the key k = (a, b):
   Ek : Alphabet -> Alphabet, \x. ax + b mod m

 * Decryption function Dk for the key k = (a', b):
   Dk : Alphabet -> Alphabet, \x. a' * (x - b) mod m
-}

module Criptografia.Affine
  ( Key
  , encrypt
  , decrypt
  ) where

import Data.Modular
import qualified Data.Vector.Unboxed as V

type Key = (Int/26, Int/26)

alphabet :: V.Vector Char
alphabet = V.enumFromTo 'A' 'Z'

encrypt :: Key -> Char -> Char
encrypt (a, b) c
  = case V.elemIndex c alphabet of
      Just x  -> alphabet V.! unMod (a * toMod x + b)
      Nothing -> c

decrypt :: Key -> Char -> Char
decrypt (a, b) c
  = case V.elemIndex c alphabet of
      Just x  -> alphabet V.! unMod (inv a * (toMod x - b))
      Nothing -> c
