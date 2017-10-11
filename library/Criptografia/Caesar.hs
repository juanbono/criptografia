{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Criptografia.Caesar
  (caesar) where

import Criptografia.Cipher
import Data.Modular
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Sized as Vec

type LatinAlphabet = Vec.Vector V.Vector 26 Char
type Key = Int/26

caesarAlphabet :: LatinAlphabet
caesarAlphabet = fromJust . Vec.toSized $ V.enumFromTo 'A' 'Z'

caesar :: Cipher LatinAlphabet Key Char
caesar = MkCipher
  { _alphabet = caesarAlphabet
  , _encrypt  = caesarEncrypt
  , _decrypt  = caesarDecrypt
  }

caesarEncrypt :: Key -> Char -> Char
caesarEncrypt n c =
  case Vec.elemIndex c caesarAlphabet of
    Just i  -> caesarAlphabet `Vec.index` ind
      where ind = fromIntegral $ unMod (toMod i + n)
    Nothing -> c

caesarDecrypt :: Key -> Char -> Char
caesarDecrypt n c =
  case Vec.elemIndex c caesarAlphabet of
    Just i  -> caesarAlphabet `Vec.index` ind
      where ind = fromIntegral $ unMod (toMod i - n)
    Nothing -> c

cipherWith :: Int -> Char -> Char
cipherWith n = caesarEncrypt (toMod n)

decipherWith :: Int -> Char -> Char
decipherWith n = caesarDecrypt (toMod n)
