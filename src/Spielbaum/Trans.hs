module Spielbaum.Trans where

import Graph.Viz 
import Graph.Graph
import Spielbaum.Type

-- If a node in a graph contains a mixture of Char and Num
-- it is not displayed by graphViz. 
-- Reason:
-- GVNID may not contain any alpha-numeric character.
-- Solution:      
-- alpha-numerics are replaced by '0'
-- Additional:
-- showText transforms '\n' to "\\n"
-- myTrans :: String -> GVTrans SpielbaumLabel
myTrans options = GVTrans
	{ getGVProg    = Default
	, getGVFormat  = "ps"
	, isGVDirected = True
	, getGVNID     = create_GVNID.showText.(create_labels "l")
	, getGVNName   = create_GVNLabel.showText.(create_labels options)
	, getGVNLabel  = Nothing
	, getGVNColor  = Nothing
	, getGVNXAtts  = Nothing
	, getGVELabel  = Nothing
	, getGVEXAtts  = Nothing
	}

-- this function removes non-digits from GVNID as explained above
create_GVNID :: String -> String
create_GVNID str 
	| str == []          = []
	| (isDigit $ head str) = (head str):(create_GVNID $ tail str)
	| otherwise          = ('0')     :(create_GVNID $ tail str)
        where isDigit '0' = True
              isDigit '1' = True
              isDigit '2' = True
              isDigit '3' = True
              isDigit '4' = True
              isDigit '5' = True
              isDigit '6' = True
              isDigit '7' = True
              isDigit '8' = True
              isDigit '9' = True
              isDigit _   = False
              
-- correction of  ShowText (Graph.Viz)
-- otherwise '\n' becomes "\\n" and so on
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

-- use options and show what is expected
create_labels :: String -> SpielbaumLabel -> String
create_labels options sL = case (options) of
           "gv" -> (label sL) ++ "\n" ++ (show $ gv sL)
           "gr" -> (label sL) ++ "\n" ++ (show $ grundy sL)
           "gg" -> (show $ gv sL) ++ " : " ++ (show $ grundy sL)
           "no" -> ("")
           "rr" -> (show $ grundy sL)
           "vv" -> (show $ gv sL)
           "l"  -> (label sL)
           _    -> (label sL) ++ "\n" ++ (show $ gv sL) ++ " : " ++ (show $ grundy sL)

