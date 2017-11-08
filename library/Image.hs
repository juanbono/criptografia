module Image
  ( readImage
  , writeImage
  , unpackWord64
  , packWord64
  ) where

import qualified Data.ByteString as BS
import Criptografia.Mugi.Internal (toWord64List, toWord8List)
import Data.Word
import Codec.BMP

-- | Width and height
type Header = (Int, Int)

unsafeReadBMP :: FilePath -> IO BMP
unsafeReadBMP filename
  = do
  res <-  readBMP filename
  case res of
    Left e -> error (show e)
    Right bmp -> return bmp

readImage :: FilePath -> IO (BS.ByteString, Header)
readImage filename = do
  bmp <- unsafeReadBMP filename
  let rgba =  unpackBMPToRGBA32 bmp
  let dims = bmpDimensions bmp
  pure (rgba, dims)

writeImage :: FilePath -> (BS.ByteString, Header) -> IO ()
writeImage f (bs, (w, h))
  = writeBMP f bmp
  where
    bmp = packRGBA32ToBMP w h bs

unpackWord64 :: BS.ByteString -> [Word64]
unpackWord64 = toWord64List . BS.unpack

packWord64 :: [Word64] -> BS.ByteString
packWord64 = BS.pack . toWord8List
