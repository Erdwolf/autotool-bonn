
module Grammatik.DPL_CYK 

-- $Id$

( accepted, accepteds )

where

-- $Log$
-- Revision 1.1  2002-12-17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1.1.1  2002/05/24 10:46:47  challenger
-- start
--
-- Revision 1.2  2001/11/29 13:41:20  autotool
-- CF_advanced: besserer parser, h�rtere tests
--
-- Revision 1.1  2001/11/28 07:27:11  autotool
-- benutze DPL in CYK/CF_Chomsky
--



import Grammatik.Chomsky


-- eins der beiden ausw�hlen:

-- benutzt FiniteMap (besser f�r hugs?)
-- import CYK_Tables

-- benutzt Arrays (auf jeden fall besser f�r ghc -O)

import Grammatik.CYK_Arrays

-- end auswahl


import Util.DPL_Set

import Ix

-- accepted :: Ix a => Chomsky a -> String -> Bool
accepted :: Chomsky Int -> String -> Bool
accepted ch w = 
    let c = ctable $ rules ch
	v = vtable $ rules ch
	top = value (ctable_lookup c) (vtable_lookup v) w 
    in	start ch `elementOf` top

-- accepteds :: Ix a => Chomsky a -> [ String ] -> [ (String, Bool) ]
accepteds :: Chomsky Int -> [ String ] -> [ (String, Bool) ]
accepteds ch ws = do
    let c = ctable $ rules ch
    let v = vtable $ rules ch
    (w, top) <- values (ctable_lookup c) (vtable_lookup v) 
	     $ filter (not . null) ws
    return (w, start ch `elementOf` top)



