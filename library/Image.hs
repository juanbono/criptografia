module Image (readImage, writeImage) where

import qualified Data.ByteString as BS
import Codec.BMP

-- | Width and height
type Header = (Int, Int)

readImage :: FilePath -> IO (BS.ByteString, Header)
readImage filename = do
  Right bmp <- readBMP filename
  let rgba =  unpackBMPToRGBA32 bmp
  let dims = bmpDimensions bmp
  pure (rgba, dims)

writeImage :: FilePath -> (BS.ByteString, Header) -> IO ()
writeImage f (bs, (w, h))
  = writeBMP f bmp
  where
    bmp = packRGBA32ToBMP w h bs
