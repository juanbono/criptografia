module Image (readImage) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Codec.BMP

readImage :: FilePath -> IO (C8.ByteString, (Int, Int))
readImage filename = do
  Right bmp <- readBMP filename
  let rgba = BS.fromStrict . unpackBMPToRGBA32 $ bmp
  let dims = bmpDimensions bmp
  pure (rgba, dims)
