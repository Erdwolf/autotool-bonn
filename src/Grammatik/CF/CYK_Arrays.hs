module Grammatik.CF.CYK_Arrays

-- -- $Id$


( module Simple_Set

, ctable
, vtable
-- , table_lookup
, ctable_lookup
, vtable_lookup
)

where


import Grammatik.CF.Chomsky
import qualified Grammatik.CF.CYK_Tables as C

import Data.FiniteMap
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

grenzen :: ( Ord a, Bounded a ) => [ a ] -> ( a, a )
grenzen [] = ( minBound , maxBound ) -- UGLY UGLY
grenzen ks = (minimum ks, maximum ks) 

ctable :: Ord a => Rules a -> Array Char (Set a)
ctable rules = 
    let fm = C.ctable rules
	ks = keysFM fm
	b = grenzen ks
    in	fmToArray b emptySet fm

vtable :: ( Bounded a, Ix a ) 
       =>  Rules a -> Array (a, a) (Set a)
vtable rules = 
    let ks = S.setToList $ vars rules
	(lo, hi) = grenzen ks
	b = ((lo,lo), (hi,hi))
	fm = C.vtable rules
    in	fmToArray b emptySet fm

fmToArray :: (Ord a, Ix a) => (a, a) -> b -> FiniteMap a b -> Array a b
fmToArray b def fm = accumArray ( \ _ n -> n ) def b $ fmToList fm






    