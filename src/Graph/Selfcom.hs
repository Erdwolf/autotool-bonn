{-# LANGUAGE DeriveDataTypeable #-}
module Graph.Selfcom where

--  $Id$

import Graph.Util
import Graph.Iso

import Autolib.Graph.Ops ( complement )
import Autolib.Graph.Basic

import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import Data.Typeable

data Selfcom = Selfcom deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Selfcom where
    scoringOrder _ = Increasing

instance C.Partial Selfcom Int ( Graph Int, FiniteMap Int Int ) where

    describe p i = vcat
        [ text "Gesucht ist ein regulärer selbstkomplementärer Graph"
	, text "mit wenigstens" <+> toDoc i <+> text "Knoten."
	, text "Sie sollen auch die passende Isomorphie angeben."
	]

    initial p i = ( circle [ 1 .. i ]
		  , listToFM $ zip [ 1 .. i ] $ reverse [ 1 .. i ]
		  )

    partial p i (g, f) = validate g
    
    total p i (g, f) = do
        inform $ vcat [ text "Der Graph ist" , nest 4 $ toDoc g ]
        let h = complement g
        inform $ vcat [ text "Das Komplement ist" , nest 4 $ toDoc h ]
	check_iso f g h
        inform $ text "Dieser Graph ist selbstkomplementär."
	check_reg g
        assert ( size g >= i )
	       $ text "Ist der Graph groß genug?"

instance Size ( Graph Int, FiniteMap Int Int ) where 
    size ( g, f ) = size g

make :: Make
make = direct Selfcom ( 6 :: Int )

--------------------------------------------------------------------------



