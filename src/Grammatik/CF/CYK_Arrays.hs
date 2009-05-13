module Grammatik.CF.CYK_Arrays

( module Autolib.Simple_Set

, ctable
, vtable
-- , table_lookup
, ctable_lookup
, vtable_lookup
)

where


import Grammatik.CF.Chomsky
import qualified Grammatik.CF.CYK_Tables as C

import Autolib.FiniteMap
import Autolib.Simple_Set
import qualified Autolib.Set as S

import Data.Array

-- | if not in range, default to empty set
safe_set_lookup :: Ix a => Array a ( Set b ) -> a -> Set b
safe_set_lookup a i = 
    if inRange (bounds a) i
    then a ! i
    else emptySet

-- polymorph ==> frißt rechenzeit
table_lookup :: Ix a 
             => Array a (Set b) -> a -> Set b
table_lookup = safe_set_lookup

ctable_lookup :: Array Char (Set b) 
              -> Char -> Set b
ctable_lookup = safe_set_lookup

vtable_lookup :: Array (Int, Int) (Set b) 
              -> (Int, Int) -> Set b
vtable_lookup = safe_set_lookup

grenzen :: ( Ord a, Bounded a ) 
        => [ a ] -> ( a, a )
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

fmToArray :: (Ord a, Ix a) 
          => (a, a) -> b -> FiniteMap a b 
          -> Array a b
fmToArray b def fm = 
    accumArray ( \ _ n -> n ) def b $ fmToList fm
