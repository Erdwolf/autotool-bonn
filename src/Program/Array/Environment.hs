{-# OPTIONS -fglasgow-exts #-}

module Program.Array.Environment 

( Environment
, empty, lookup -- , example
, add
, must_be_equal
, contents, make
)

where

import Prelude hiding ( lookup )

import Program.Array.Value
import Program.Array.Program
import Program.Array.Statement

import Autolib.FiniteMap
import Autolib.Set
import Autolib.TES.Identifier

import Autolib.Reporter
import Autolib.ToDoc hiding ( empty )
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Environment = Environment ( FiniteMap Identifier Value )
    deriving ( Typeable, Eq )

contents ( Environment e ) = fmToList e

make kvs = Environment $ listToFM kvs

instance Size Environment where size e = 0

empty :: Environment
empty = Environment $ emptyFM

lookup :: Environment -> Identifier -> Maybe Value
lookup ( Environment e ) = lookupFM e

add :: Environment -> Identifier -> Value -> Environment
add ( Environment e ) k v = Environment $ addToFM e k v

example :: Environment
example = Environment $ listToFM
    [ ( read "x", read "{4,1,2}" ) ]

pform :: Environment -> Program
pform ( Environment e ) = Program $ do
    ( k, v ) <- fmToList e
    return $ Declare k ( depth v ) v

instance ToDoc Environment where
    toDoc = toDoc . pform

instance Reader Environment where
    reader = do
        ds <- many declaration
	return $ Environment $ listToFM $ do
	    Declare name depth value <- ds
	    return ( name, value )

must_be_equal :: Environment -> Environment -> Reporter ()
must_be_equal ( Environment e ) ( Environment f ) = do
    let ke = keysFM e
	kf = keysFM f
    when ( ke /= kf ) $ reject $ vcat
	 [ text "Mengen der gebundenen Variablen stimmen nicht überein:"
	 , nest 4 $ vcat [ toDoc ke, toDoc kf ]
	 ]
    sequence_ $ do
	 ((p, v) , (q, w)) <- zip ( fmToList e ) ( fmToList f )
	 return $ do
	     when ( p /= q ) $ reject 
		  $ text "falsche Reihenfolge" <+> toDoc (p,q)
	     when ( v /= w ) $ reject $ vcat
	         [ text "Werte für Variable" <+> toDoc p
	            <+> text "stimmen nicht überein:"
	         , nest 4 $ vcat [ toDoc v, toDoc w ]
	         ]
