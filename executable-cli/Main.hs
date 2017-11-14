module Main where

import Criptografia
import Options
import Data.Monoid ((<>))
import Control.Lens (over, both)
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- parseArgs

  -- extract the secret key and init vector
  let (key, initv) = over both fromString (secretKey args, iv args)

  imageBytes <- BS.readFile (filename args)

  let (header, img) = BS.splitAt 54 imageBytes

  let encryptedImg = mugiEncrypt key initv (unpackWord64 img)

  BS.writeFile (filename args) (header <> packWord64 encryptedImg)
