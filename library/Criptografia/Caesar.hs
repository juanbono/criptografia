{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Criptografia.Caesar
  (caesar) where

import Criptografia.Cipher
import Data.Modular
import qualified Data.Vector.Unboxed as V

caesar :: Cipher (V.Vector Char) (Int/26) Char
caesar = MkCipher
  { _alphabet = caesarAlphabet
  , _encrypt  = caesarEncrypt
  , _decrypt  = caesarDecrypt
  }

caesarAlphabet :: V.Vector Char
caesarAlphabet = V.enumFromTo 'A' 'Z'

caesarEncrypt :: Int/26 -> Char -> Char
caesarEncrypt n c =
  case V.elemIndex c caesarAlphabet of
    Just i  -> caesarAlphabet V.! unMod (toMod i + n)
    Nothing -> c

caesarDecrypt :: Int/26 -> Char -> Char
caesarDecrypt n c =
  case V.elemIndex c caesarAlphabet of
    Just i  -> caesarAlphabet V.! unMod (toMod i - n)
    Nothing -> c

cipherWith :: Int -> Char -> Char
cipherWith n = caesarEncrypt (toMod n)

decipherWith :: Int -> Char -> Char
decipherWith n = caesarDecrypt (toMod n)
