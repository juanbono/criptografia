{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Criptografia.Caesar
  ( cipherWith
  , decipherWith
  ) where

import Data.Modular
import qualified Data.Vector.Unboxed as V

alphabet :: V.Vector Char
alphabet = V.enumFromTo 'A' 'Z'

encrypt :: Int/26 -> Char -> Char
encrypt n c =
  case V.elemIndex c alphabet of
    Just i  -> alphabet V.! unMod (toMod i + n)
    Nothing -> c

decrypt :: Int/26 -> Char -> Char
decrypt n c =
  case V.elemIndex c alphabet of
    Just i  -> alphabet V.! unMod (toMod i - n)
    Nothing -> c

cipherWith :: Int -> Char -> Char
cipherWith n = encrypt (toMod n)

decipherWith :: Int -> Char -> Char
decipherWith n = decrypt (toMod n)
