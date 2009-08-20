{-# OPTIONS -Onot -fglasgow-exts #-}


{-# LANGUAGE TemplateHaskell #-}
module SAT.Param where

--   $Id$

import SAT.Types

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Param = 
     Param { vars :: Set Variable
	   , clauses :: Int -- ^ anzahl (empfohlen: 3.5 * variablen)
	   }
     deriving ( Typeable )


p :: Int -> Param
p n = let f = 3.5 :: Double
      in  Param { vars = mkSet $ take n $ do c <- [ 'p' .. ] ; return $ read [c]
	      , clauses = round $ f * fromIntegral n 
	      }


$(derives [makeReader, makeToDoc] [''Param])
-- {-! for Param derive: ToDoc, Reader !-}

-- local variables:
-- mode: haskell
-- end:
