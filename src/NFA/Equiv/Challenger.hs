module NFA.Equiv.Challenger where

-- -- $Id$

import NFA.Type
import NFA.Equiv.Core

import ToDoc
import Reporter
import Informed
import Sets
import Util.Size
import Letters


import qualified Challenger as C

data Equiv = Equiv deriving ( Eq, Ord, Show, Read )

instance Size (Trenner c s) where size _ = 1
       

instance C.Partial Equiv ( NFA Char Int ) [[ Trenner Char Int ]] where
    describe p i = vcat $ 
	 [ text "Sie sollen den deterministischen Automaten minimieren,"
	 , text "indem Sie Paare von nicht äquivalenten Zuständen angeben."
         , text "In Ihrer Einsendung [ l_0, l_1, .. l_n ]"
	 , text "soll jeweils l_k die Liste aller Tripel (p, q, c) sein,"
	 , text "für die p \\sim_k q, aber T(p,c) \\not\\sim_k T(q,c)."
	 , text "Die letzte Unter-Liste  l_n  soll leer ( = [] ) sein."
         , text "Der Automat ist:"
	 , nest 4 $ toDoc i
	 ]
       
    initial p i =
        let p : q : r : s : _ = setToList $ states i
	    a : b :     _ = setToList $ letters i
	in  [ [ (p, q, a), (r, s, b) ]
	    , [ (p, s, a) ]
	    ]
    partial p i b = do
        foldM ( schritt ( letters i ) i ) ( start i )
	      $ zip [ 0 .. ] b
	return ()

    total p i b = do
	assert  ( not ( null b ) && null ( last b ) )
           $ vcat 
	       [ text "Ist Ihre Liste vollständig?"
	       , nest 4 $  text "Die letzte Unter-Liste soll [] sein:"
			<+> text "[ [ ... ] , ... , [] ]"
	       ]
