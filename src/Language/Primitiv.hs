--- $Header$

module Language.Primitiv where

import Language.Type
import Autolib.Set
import Power

prim :: Language
prim = Language
	{ abbreviation = "{ 0,1 }^* - { w^k : w in {0,1}^*, k>1 }"
	, alphabet     = mkSet "01"
	, contains     = is_prim
	, sample       = random_sample prim
	}

is_prim :: String -> Bool
is_prim "" = False
is_prim w  = and $ do
    k <- [ 2 .. (length w) ]
    return $ not $ is_power k w
 





