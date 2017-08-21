module Main where

import Criptografia
import Conduit
import Data.Modular
import Data.Char (toUpper, toLower)
import Lens.Micro.Platform

caesarWithLength1 :: Char -> Char
caesarWithLength1 = caesar^.encrypt $ toMod 1

main :: IO ()
main
  = runConduitRes
  $ sourceFile "input.txt"
 .| decodeUtf8C
 .| omapCE toUpper
 .| omapCE caesarWithLength1
 .| omapCE toLower
 .| encodeUtf8C
 .| sinkFile "input2.txt"

