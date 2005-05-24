-- $Id$

module RM.Memory where

import RM.Type ( Register )

import Autolib.FiniteMap ( FiniteMap , emptyFM , lookupWithDefaultFM , addToFM
			 , listToFM , delFromFM
			 )

-------------------------------------------------------------------------------

type Memory = FiniteMap Register Integer

empty :: Memory
empty = emptyFM

get :: Memory -> Register -> Integer
get q = lookupWithDefaultFM q 0

set :: Memory -> Register -> Integer -> Memory
set = addToFM

fromList :: [ ( Register , Integer ) ] -> Memory
fromList = listToFM

inc, dec :: Memory -> Register -> Memory
inc = genc succ
dec = genc ( \ x -> max 0 (pred x) )

genc :: (Integer -> Integer) -> Memory -> Register -> Memory
genc f m n = set m n $ f $ get m n
