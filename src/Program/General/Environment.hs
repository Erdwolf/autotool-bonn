{-# OPTIONS -fglasgow-exts #-}

module Program.General.Environment 

( Environment, Value(..)
, empty, lookup
, add
, must_be_equal
, contents, make, mapM
)

where

import Program.General.Value

import Autolib.FiniteMap
import Autolib.Set
import Autolib.TES.Identifier

import Autolib.Reporter hiding ( mapM )
import Autolib.ToDoc hiding ( empty )
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Prelude hiding ( mapM, lookup )

data Environment val = Environment ( FiniteMap Identifier val )
    deriving ( Typeable, Eq )

contents ( Environment e ) = fmToList e

make kvs = Environment $ listToFM kvs

instance Size ( Environment val ) where size e = 0

empty :: Environment val
empty = Environment $ emptyFM

lookup :: Environment val -> Identifier -> Maybe val
lookup ( Environment e ) = lookupFM e

add :: Environment val -> Identifier -> val -> Environment val
add ( Environment e ) k v = Environment $ addToFM e k v

mapM :: Monad m 
     => ( v -> m w )
     -> Environment v 
     -> m ( Environment w )
mapM f env = do
    binds <- sequence $ do
        ( k, v ) <- contents env
        return $ do
            w <- f v
            return ( k, w )
    return $ make binds

instance Value val => ToDoc ( Environment val ) where
    toDoc ( Environment e ) = vcat $ do
        ( k, v ) <- fmToList e
        return $ hsep [ typeform v , toDoc k, equals, toDoc v, semi ]

instance Value val => Reader ( Environment val ) where
    reader = fmap make $ many $ do
        ty <- typeread 
        id <- reader
        my_equals
        val <- ty 
        my_semi
        return ( id, val )


must_be_equal :: ( ToDoc val, Eq val ) 
    => Environment val -> Environment val -> Reporter ()
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
