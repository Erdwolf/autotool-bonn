-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Encryption.Utils
-- Copyright   :  (c) Dominic Steinitz 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  dominic.steinitz@blueyonder.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities to translate between Integral and lists of Octet.
--
-----------------------------------------------------------------------------

module Codec.Encryption.Utils (
   -- * Types
   Octet,
   -- * Function types
   toOctets, fromOctets,
   i2osp
	      ) where

import Word
import Bits

powersOf n = 1 : (map (*n) (powersOf n))

toBase x = 
   map fromIntegral .
   reverse .
   map (flip mod x) .
   takeWhile (/=0) .
   iterate (flip div x)

-- | Take a number a convert it to base 256 as a list of octets.

toOctets :: Integral a => a -> [Octet]
toOctets x = (toBase 256 . fromIntegral) x

type Octet = Word8

-- | Take a list of octets (a number expressed in base 256) and convert it
--   to a number.

fromOctets :: Num a => [Octet] -> a
fromOctets x = 
   fromIntegral $ sum $ zipWith (*) (powersOf 256) 
                     (reverse (map fromIntegral x))

-- | Take the length of the required number of octets and convert the 
--   number to base 256 padding it out to the required length. If the
--   required length is less than the number of octets of the converted
--   number then return the converted number. NB this is different from
--   the standard <ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1.pdf>
--   but mimics how replicate behaves.

i2osp :: Integral a => Int -> a -> [Octet]
i2osp l y = 
   pad ++ z
      where
         pad = replicate (l - unPaddedLen) (0x00::Octet)
	 z = toOctets y
	 unPaddedLen = length z
