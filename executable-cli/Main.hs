module Main where

import Criptografia
import CLI
import Options.Applicative (execParser)

main :: IO ()
main = do
  -- parse arguments
  cmd <- execParser opts
  -- extract the secret key and init vector
  let (key, initv) = (fromString . secretKey $ cmd, fromString . iv $ cmd)
  -- read the image
  (img, header) <- readImage (filename cmd)

  let encryptedList = mugiEncrypt key initv (unpackWord64 img)

  -- write the image using the same header
  writeImage (filename cmd) (packWord64 encryptedList, header)
