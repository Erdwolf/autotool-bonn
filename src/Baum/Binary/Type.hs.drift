{-# OPTIONS -fglasgow-exts #-}

module Baum.Binary.Type where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Baum a = Null
	    | Baum { key :: a
		   , left :: Baum a
		   , right :: Baum a
		   }
     deriving ( Typeable, Eq )

instance Functor Baum where
    fmap f b = case b of
        Null -> Null
        Baum {} -> Baum { key = f $ key b
                        , left = fmap f $ left b
                        , right = fmap f $ right b
                        }

isNull :: Baum a -> Bool
isNull Null = True
isNull _ = False

{-! for Baum derive: Reader, ToDoc, Haskell2Xml !-}

-- local variables:
-- mode: haskell
-- end;



