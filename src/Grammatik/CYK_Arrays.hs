module Grammatik.CYK_Arrays

-- $Id$


( module Simple_Set

, ctable
, vtable
-- , table_lookup
, ctable_lookup
, vtable_lookup
)

where

-- $Log$
-- Revision 1.1  2002-12-17 15:17:58  joe
-- grammatik -> Grammatik.
--
-- Revision 1.1.1.1  2002/05/24 10:46:47  challenger
-- start
--
-- Revision 1.1  2001/11/29 13:41:20  autotool
-- CF_advanced: besserer parser, härtere tests
--
-- Revision 1.1  2001/11/28 07:27:11  autotool
-- benutze DPL in CYK/CF_Chomsky
--

import Grammatik.Chomsky
import qualified Grammatik.CYK_Tables as C

import FiniteMap
import Simple_Set
import qualified Set as S

import Array


-- polymorph ==> frißt rechenzeit
table_lookup :: Ix a => Array a b -> a -> b
table_lookup a i = a ! i

ctable_lookup :: Array Char b -> Char -> b
ctable_lookup a i = a ! i

vtable_lookup :: Array (Int, Int) b -> (Int, Int) -> b
vtable_lookup a i = a ! i

ctable :: Ord a => Rules a -> Array Char (Set a)
ctable rules = 
    let fm = C.ctable rules
	ks = keysFM fm
	b = (minimum ks, maximum ks)
    in	fmToArray b emptySet fm

vtable :: Ix a =>  Rules a -> Array (a, a) (Set a)
vtable rules = 
    let ks = S.setToList $ vars rules
	(lo, hi) = (minimum ks, maximum ks)
	b = ((lo,lo), (hi,hi))
	fm = C.vtable rules
    in	fmToArray b emptySet fm

fmToArray :: (Ord a, Ix a) => (a, a) -> b -> FiniteMap a b -> Array a b
fmToArray b def fm = accumArray ( \ _ n -> n ) def b $ fmToList fm






    