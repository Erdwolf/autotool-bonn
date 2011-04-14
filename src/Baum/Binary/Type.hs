{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Baum.Binary.Type where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

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

$(derives [makeReader, makeToDoc] [''Baum])

-- local variables:
-- mode: haskell
-- end;



