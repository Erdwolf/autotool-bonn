module JVM.Memory 

( Var (..), Memory
, make, get, set
)

where

--   $Id$

import JVM.Type
import Autolib.FiniteMap
import Autolib.ToDoc

import Machine.Var

type Memory = FiniteMap Var Integer

make :: [ ( Var, Integer ) ] -> Memory
make = listToFM

get :: Memory -> Var -> Integer
get m v = lookupWithDefaultFM m 0 v

set :: Memory -> (Var, Integer) -> Memory
set m ( v, k ) = addToFM m v k
