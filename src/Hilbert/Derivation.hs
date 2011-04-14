{-# LANGUAGE DeriveDataTypeable #-}
module Hilbert.Derivation

( Derivation
, derive
, example
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

example :: Derivation
example = read $ unlines
        [ "let { Fx = sub H13 { A = x && y, B = x } "
	, "    , Px = sub H4  { A = x, B = y } "
	, "} in mopo Px Fx"
	]

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

derive axioms d = do
    e <- foldM ( \ e ( k, ( name, action ) ) -> do
		   inform $ text "Schritt" <+> toDoc k
	           v <- nested 4 $ informed_value e action
	           extend e ( name, v )
	       ) axioms ( zip [1 :: Int ..] $ contents $ env d )
    v <- informed_value e $ act d
    return v

	       


