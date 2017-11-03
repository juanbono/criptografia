{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criptografia
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import Data.LargeWord (Word128)
import Options.Applicative (execParser)
import Data.Semigroup
import CLI

toWord128 :: String -> Word128
toWord128 = fromByte128 . BS.unpack. C8.pack

main :: IO ()
main = do
  cmd <- execParser opts
  let (key, initv) = (toWord128 . secretKey $ cmd, toWord128 . iv $ cmd)
  (img, (width, height)) <- readImage (filename cmd)
  print $ "width: " <> show width <> " and height: " <> show height
  print $ "key: " <> show key <> " and iv: " <> show initv
  let stream = mugiStream $ initMugi key initv
  print $ show (take 10 stream)

{-
conduit example:
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
--}
