module Grammatik.CF.DPL_CYK 

-- $Id$

( accepted, accepteds )

where



import Grammatik.CF.Chomsky


-- eins der beiden auswählen:

-- benutzt FiniteMap (besser für hugs?)
-- import CYK_Tables

-- benutzt Arrays (auf jeden fall besser für ghc -O)

import Grammatik.CF.CYK_Arrays

-- end auswahl


import Util.DPL_Set

import Ix

-- accepted :: Ix a => Chomsky a -> String -> Bool
-- input: eine reduzierte grammatik
accepted :: Chomsky Int -> String -> Bool
accepted ch w = 
    let c = ctable $ rules ch
	v = vtable $ rules ch
	top = value (ctable_lookup c) (vtable_lookup v) w 
    in	thing ch top

-- accepteds :: Ix a => Chomsky a -> [ String ] -> [ (String, Bool) ]
-- input: eine reduzierte grammatik
accepteds :: Chomsky Int -> [ String ] -> [ (String, Bool) ]
accepteds ch ws = do
    let c = ctable $ rules ch
    let v = vtable $ rules ch
    (w, top) <- values (ctable_lookup c) (vtable_lookup v) 
	     $ filter (not . null) ws
    return ( w, thing ch top )

thing :: Chomsky Int -> Set Int -> Bool
thing ch top = if null $ rules ch
	then False
	else start ch `elementOf` top


