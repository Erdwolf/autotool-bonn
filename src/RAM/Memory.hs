module RAM.Memory where

-- $Id$

import RAM.Type
import FiniteMap
import OrdFM

type Memory = FiniteMap Var Integer

make :: [ ( Var, Integer ) ] -> Memory
make = listToFM

get :: Memory -> Var -> Integer
get m v = lookupWithDefaultFM m 0 v

set :: Memory -> (Var, Integer) -> Memory
set m ( v, k ) = addToFM m v k
