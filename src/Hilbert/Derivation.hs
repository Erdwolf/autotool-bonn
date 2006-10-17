{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Hilbert.Derivation

( Derivation
, derive
)

where

import Hilbert.Env
import Hilbert.Actions

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Autolib.FiniteMap
import Autolib.TES.Identifier
import Autolib.TES
import Autolib.Size

import Data.Typeable

data Derivation =
     Derivation { env :: Env Action 
		, act :: Action
		}
    deriving Typeable

instance Size Derivation where 
    size = size . env

instance ToDoc Derivation where
    toDoc d = vcat [ text "let" <+> toDoc ( env d )
		   , text "in"  <+> toDoc ( act d )
		   ]

instance Reader Derivation where
    reader = do
        my_reserved "let"
	e <- reader
	my_reserved "in"
	r <- reader
	return $ Derivation { env = e, act = r }

derive d = do
    e <- foldM ( \ e ( name, action ) -> do
	           v <- value e action
	           extend e ( name, v )
	       ) ( Hilbert.Env.empty ) ( contents $ env d )
    v <- value e $ act d
    return v

	       


