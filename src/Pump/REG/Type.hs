-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Pump.REG.Type where

--   $Id$

import Autolib.Size
import Autolib.Hash
import Autolib.Reader
import Autolib.ToDoc


import Autolib.Util.Splits
import Control.Monad (guard)
import Data.Typeable

data Zerlegung = Zerlegung
	       { u :: String, v :: String, w :: String }
     deriving (Eq, Ord, Typeable)

$(derives [makeReader, makeToDoc] [''Zerlegung])
-- {-! for Zerlegung derive: ToDoc, Reader !-}

instance Hash Zerlegung where
    hash z = hash ( u z, v z, w z )

--------------------------------------------------------------------------

-- | alle Zerlegungen mit |uv| <= n, |v| > 0
zerlegungen :: String -> Int -> [ Zerlegung ]
zerlegungen p n = do
    ( ab, c ) <- take n $ splits p
    ( a, b  ) <- splits ab
    guard $ not (null b)
    return $ Zerlegung { u = a, v = b, w = c }

aufpump :: Zerlegung -> Int -> String
aufpump g i =  u g
	 ++ concat ( replicate i $ v g )
	 ++ w g





