module Grammatik.CF.DPL_CYK 

-- $Id$

( accepted, accepteds )

where

-- $Log$
-- Revision 1.3  2003-11-25 11:33:59  joe
-- Checker umgestellt
-- Chomsky nach subdir CF/
--
-- Revision 1.2  2003/11/25 09:49:49  joe
-- aufgaben zu grammatiken
--
-- Revision 1.1  2003/11/25 08:21:09  joe
-- moved CF-related files into subdir
--
-- Revision 1.1  2002/12/17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1.1.1  2002/05/24 10:46:47  challenger
-- start
--
-- Revision 1.2  2001/11/29 13:41:20  autotool
-- CF_advanced: besserer parser, härtere tests
--
-- Revision 1.1  2001/11/28 07:27:11  autotool
-- benutze DPL in CYK/CF_Chomsky
--



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



