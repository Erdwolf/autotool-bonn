module Grammatik.CF.DPL_CYK 

-- -- $Id$

( accepted, accepteds )

where



import Grammatik.CF.Chomsky


-- eins der beiden ausw�hlen:

-- benutzt FiniteMap (besser f�r hugs?)
-- import CYK_Tables

-- benutzt Arrays (auf jeden fall besser f�r ghc -O)

import Grammatik.CF.CYK_Arrays

-- end auswahl


import Util.DPL_Set
import Data.List (partition)

-- import Ix

accepted :: Chomsky Int -> String -> Bool
accepted ch w = 
    let c = ctable $ rules ch
	v = vtable $ rules ch
	top = value (ctable_lookup c) (vtable_lookup v) w 
    in	if null w then eps ch
        else thing ch top

accepteds :: Chomsky Int -> [ String ] -> [ (String, Bool) ]
accepteds ch ws =
    let ( small, large ) = partition null ws
	xsmall = do 
	    w <- small
	    return ( w, eps ch )
        xlarge = do
	    let c = ctable $ rules ch
		v = vtable $ rules ch
	    (w, top) <- values (ctable_lookup c) (vtable_lookup v) large
	    return ( w, thing ch top )
    in	xsmall ++ xlarge

thing :: Chomsky Int -> Set Int -> Bool
thing ch top = if null $ rules ch
	then False
	else start ch `elementOf` top


