module NFA.Equiv.Challenger where

--  $Id$

import Autolib.NFA.Type
import Autolib.Exp.Inter
import Autolib.Exp.Env

import NFA.Equiv.Core
import NFA.Equiv.Conf hiding ( alphabet )
import NFA.Equiv.Generate

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Set
import Autolib.Size
import Autolib.Xml

import Autolib.Dot.Dotty ( peng )

import Data.Typeable

import qualified Challenger as C
import Inter.Types
import Inter.Quiz

data Equiv = Equiv deriving ( Eq, Ord, Show, Read, Typeable )

instance Size (Trenner c s) where size _ = 1
       
instance C.Partial Equiv ( NFA Char Int ) [[ Trenner Char Int ]] where

    report p i = do
         inform $ vcat $ 
	     [ text "Sie sollen den deterministischen Automaten minimieren,"
	     , text "indem Sie Paare von nicht äquivalenten Zuständen angeben."
	     , text ""
             , text "In Ihrer Einsendung [ l_0, l_1, .. l_n ]"
	     , text "soll jeweils l_k die Liste aller Tripel (p, q, c) sein,"
	     , text "für die p \\sim_k q, aber T(p,c) \\not\\sim_k T(q,c)."
	     , text ""
	     , text "Die letzte Unter-Liste  l_n  soll leer ( = [] ) sein."
	     , text ""
             , text "Der Automat ist:"
	     , nest 4 $ toDoc i
	     ]
	 peng i

    initial p i =
        let p : q : r : s : _ = lstates i
	    a : b :         _ = setToList $ alphabet i
	in  [ [ (p, q, a), (r, s, b) ]
	    , [ (p, s, a) ]
	    ]

    partial p i b = do
        foldM ( schritt ( alphabet i ) i ) ( start i )
	      $ zip [ 0 .. ] b
	return ()

    total p i b = do
	assert  ( not ( null b ) && null ( last b ) )
           $ vcat 
	       [ text "Ist Ihre Liste vollständig?"
	       , nest 4 $  text "Die letzte Unter-Liste soll [] sein:"
			<+> text "[ [ ... ] , ... , [] ]"
	       ]

instance Container (Trenner c s) ((s, s), c) where
    label _ = "Trenner"
    pack (s1, s2, c) = ((s1, s2), c)
    unpack ((s1, s2), c) = (s1, s2, c)
    

make :: Make
make = direct Equiv 
     $ inter (std_sigma "ab") $ read "All aabba All"


instance Generator Equiv Conf  ( NFA Char Int , [ Klassen Int ] )  where
    generator p conf key = roll conf

instance Project Equiv  ( NFA Char Int , [ Klassen Int ] ) 
                        ( NFA Char Int ) where
    project p ( d, xsss ) = d


qmake :: Make
qmake = quiz Equiv $ NFA.Equiv.Conf.example
      
