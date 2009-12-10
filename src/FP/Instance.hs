{-# LANGUAGE TemplateHaskell #-}

module FP.Instance where

--  $Id$

import FP.Type
import FP.Env

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size 

import Autolib.TES.Identifier
import Autolib.Xml
import Autolib.Hash
import Autolib.Size

import Data.Typeable


data TI = TI { target :: Type
	     , signature :: Env
	     }
    deriving  ( Typeable )

example :: TI
example = TI { target = read "forall a b . a -> (b -> a)"
	, signature = read $ unlines
		    [ "{ flip :: forall a b c . (a -> b -> c) -> (b -> a -> c)"
		    , "; id :: forall a . a -> a"
		    , "}"
		    ]
	}

$(derives [makeReader, makeToDoc] [''TI])


-- local variables:
-- mode: haskell
-- end:
