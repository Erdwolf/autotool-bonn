module Boolean.Eval where

--  $Id$

import Boolean.Data
import Boolean.Op ( inter )

import Data.FiniteMap
import Autolib.Util.Wort
import Autolib.Sets

type Belegung v = FiniteMap v Bool

belegungen :: Ord v 
	  => Set v
	  -> [ Belegung v ]
belegungen vs = do
    xs <- alle [ False, True ] $ cardinality vs
    return $ listToFM $ zip ( setToList vs ) xs

eval :: Belegung Identifier -> Exp -> Bool
eval b = 
    tfold ( lookupWithDefaultFM b (error "Boolean.Eval") )
	  ( inter )
