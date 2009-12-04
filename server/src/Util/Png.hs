-- quick and dirty size extraction from PNG files
-- see also http://www.w3.org/TR/PNG/

module Util.Png (pngSize) where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

pngSize :: S.ByteString -> (Int, Int)
pngSize = runGet pngSize' . L.fromChunks . (:[])

pngSize' :: Get (Int, Int)
pngSize' = do
    fileHeader
    imgHeader

fileHeader :: Get ()
fileHeader = do
    0x89504E47 <- getWord32be
    0x0D0A1A0A <- getWord32be
    return ()

imgHeader :: Get (Int, Int)
imgHeader = do
    13 <- getWord32be
    0x49484452 <- getWord32be
    w <- getWord32be
    h <- getWord32be
    return (fromIntegral w, fromIntegral h)
