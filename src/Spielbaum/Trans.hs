module Spielbaum.Trans where

import Graph.Viz 
import Graph.Graph
-- import Testgraph
import Spielbaum.Type

-- myTrans instead of instanzTrans
--
-- If a node in a graph contains a mixture of Char and Num
-- it is not displayed by graphViz. 
-- Reason:
-- GVNID may not contain any alpha-numeric character.
-- Solution:      
-- alpha-numerics are replaced by '0'
-- Additional:
-- showText transforms '\n' to "\\n"
-- I avoid this by replacing ':' by '\n' afterwards
myTrans :: ShowText knoten => GVTrans knoten
myTrans = GVTrans
	{ getGVProg    = Default
	, getGVFormat  = "ps"
	, isGVDirected = True
	, getGVNID     = create_GVNID.showText
--	, getGVNName   = create_String.create_GVNLabel.showText
	, getGVNName   = create_GVNLabel.showText 
	, getGVNLabel  = Nothing
	, getGVNColor  = Nothing
	, getGVNXAtts  = Nothing
	, getGVELabel  = Nothing
	, getGVEXAtts  = Nothing
	}

create_GVNID :: String -> String
create_GVNID str 
	| str == []          = []
	| isDigit (head str) = (head str):(create_GVNID $ tail str)
	| otherwise          = ('0')     :(create_GVNID $ tail str)

create_GVNLabel :: String -> String
create_GVNLabel str = 
 	let ( pre , post ) = span (/= '\\') str
 	in 
		if ( post == [] ) then str
		 else 
		 	let char = head $ tail post 
			in case char of
		 	 	 'n'  -> ( pre ++ "\n" ++ ( drop 2 post ) )
				 '\\' -> ( pre ++ "\\" ++ ( drop 2 post ) )
				 't'  -> ( pre ++ "\t" ++ ( drop 2 post ) )
				 '\"' -> ( pre ++ "\"" ++ ( drop 2 post ) )
				 _    -> str

create_String :: SpielbaumLabel -> String
create_String sL = label sL
