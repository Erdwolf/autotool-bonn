module JVM.Memory where

-- $Id$

import JVM.Type
import Data.FiniteMap
import OrdFM

type Var = Integer -- Adresse

type Memory = FiniteMap Var Integer

make :: [ ( Var, Integer ) ] -> Memory
make = listToFM

get :: Memory -> Var -> Integer
get m v = lookupWithDefaultFM m 0 v

set :: Memory -> (Var, Integer) -> Memory
set m ( v, k ) = addToFM m v k
