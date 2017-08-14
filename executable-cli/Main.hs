module Main where
import qualified Criptografia as Caesar

import Conduit
import Data.Char (toUpper, toLower)

main :: IO ()
main
  = runConduitRes
  $ sourceFile "input.txt"
 .| decodeUtf8C
 .| omapCE toUpper
 .| omapCE (Caesar.cipherWith 1)
 .| omapCE toLower
 .| encodeUtf8C
 .| sinkFile "input2.txt"

