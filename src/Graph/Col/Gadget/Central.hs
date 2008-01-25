{-# language DeriveDataTypeable, MultiParamTypeClasses #-}

module Graph.Col.Gadget.Central where

import Graph.Col.Gadget.Config
import Graph.Cage.Central ( extend_colourings )

import Graph.Util
import qualified Challenger as C
import Inter.Types

import Autolib.Graph.Basic ( independent )
import Autolib.Graph.Ops ( restrict )
import Autolib.Dot ( peng, Layout_Program (..) )
import qualified Autolib.Reporter.Set ( subeq )
import Autolib.Util.Wort

import Data.Typeable

data Col_Gadget = Col_Gadget 
     deriving ( Eq, Ord, Show, Read, Typeable )

instance ( GraphC a, Show a ) => 
    C.Partial Col_Gadget ( Config a ) ( Graph a ) where

    report p c = do 
        inform $ vcat
	       [ text "Geben Sie einen Graphen G"
	       , text "mit höchsten" <+> toDoc ( max_size c ) <+> text "Knoten an,"
	       , text "der die folgenden Eigenschaften erfüllt:"
	       , text "die Menge I = " <+> toDoc ( connectors c ) <+> text "in G ist unabhängig"
	       , text "und jede" <+> toDoc (colors c) <> text "-Färbung f von I"
	       , text "läßt sich genau dann auf G fortsetzen, wenn f(i) mehr als eine Farbe enthält."
	       ]

    initial p c = independent $ connectors c

    partial p c g = do
        inform $ vcat [ text "Ihr Graph ist" , nest 4 $ toDoc g ]
        peng $ g { layout_program = Dot
                 , layout_hints = [ "-Nshape=ellipse" ]
                 }

        validate g
	
	when ( length ( lknoten g ) > max_size c ) $ reject
	     $ text "Die Knotenzahl ist zu groß."

        Autolib.Reporter.Set.subeq ( text "vorgegebene Teilmenge" , connectors c )
                                   ( text "Knoten Ihres Graphen" , knoten g )
        
	let forbidden = lkanten $ restrict ( connectors c ) g
	when ( not $ null forbidden ) $ reject $ vcat
	     [ text "die vorgegebene Menge ist nicht unabhängig:"
	     , nest 4 $ toDoc forbidden
	     ]

    total p c g = sequence_ $ do
        pre <- all_colourings ( colors c ) ( connectors c ) 
	let uni = 1 == cardinality ( mkSet $ eltsFM pre )
	let cols = extend_colourings ( colors c ) g pre
	return $ case ( uni, cols ) of
	    ( True, col : _ ) -> reject $ vcat
	        [ text "Die Färbung von I"
		, nest 4 $ toDoc pre
		, text "läßt sich doch auf G fortsetzen:"
		, nest 4 $ toDoc col
		]
	    ( False, [] ) -> reject $ vcat
		[ text "Die Färbung von I"
		, nest 4 $ toDoc pre
		, text "läßt sich nicht auf G fortsetzen."
		]
	    _ -> return ()


all_colourings k xs = do
    cs <- alle [ 1 .. k ] ( cardinality xs )
    return $ listToFM $ zip ( setToList xs ) cs    

make :: Make
make = direct Col_Gadget example
