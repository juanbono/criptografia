module Main where

import Criptografia
import Options
import Data.Monoid ((<>))
import Control.Lens (over, both)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import Data.ByteString.Builder

import Data.Word

nullSeq :: [Word8]
nullSeq = BS.unpack $ BS.replicate 16 0

mugiSequence :: [Word8]
mugiSequence = concatMap toByte $ take 16 (mugiStream 0 0)

toHex :: BS.ByteString -> LazyBS.ByteString
toHex = toLazyByteString . byteStringHex

main :: IO ()
main = do
  args <- parseArgs

  -- extract the secret key and init vector
  let (key, initv) = over both fromString (secretKey args, iv args)

  imageBytes <- BS.readFile (filename args)

  let (header, img) = BS.splitAt 54 imageBytes
  LazyBS.writeFile "original hexa.txt" (toHex (header <> img))


  let encryptedImg = mugiEncrypt key initv (unpackWord64 img)
  let finalImage = header <> packWord64 encryptedImg

  LazyBS.writeFile "modified hexa.txt" (toHex finalImage)

  BS.writeFile (filename args) finalImage

