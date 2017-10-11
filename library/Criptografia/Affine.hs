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
import Data.Maybe
import qualified Data.Vector.Generic.Sized as Vec
import Criptografia.Cipher
import qualified Data.Vector as V

type LatinAlphabet = Vec.Vector V.Vector 26 Char
type Key = (Int/26, Int/26)

affineAlphabet :: LatinAlphabet
affineAlphabet = fromJust . Vec.toSized $ V.enumFromTo 'A' 'Z'

affine :: Cipher LatinAlphabet Key Char
affine = MkCipher
  { _alphabet = affineAlphabet
  , _encrypt  = affineEncrypt
  , _decrypt  = affineDecrypt
  }

affineEncrypt :: Key -> Char -> Char
affineEncrypt (a, b) c
  = case Vec.elemIndex c affineAlphabet of
      Just x  -> affineAlphabet `Vec.index` i
        where i = fromIntegral $ unMod (a * toMod x + b)
      Nothing -> c

affineDecrypt :: Key -> Char -> Char
affineDecrypt (a, b) c
  = case Vec.elemIndex c affineAlphabet of
      Just x  -> affineAlphabet `Vec.index` i
        where i = fromIntegral $ unMod (inv a * (toMod x - b))
      Nothing -> c
