module JVM.Machine where

-- -- $Id$

import Machine.Class

import JVM.Type
import JVM.Memory
import JVM.State
import JVM.Step

import Set
import Size
import Data.Array

instance Compute Program State where
    next p s = mkSet $ step s
    accepting p s =   inRange ( bounds $ code s ) ( pc s )
		  &&  Halt == code s ! pc s

instance In Program Memory State where
    input  p m = State { code = listArray (0, pred $ length p) p
		       , pc = 0
		       , stack = []
		       , memory = m
		       , schritt = 0
		       , past = [] 
		       }
instance Out Program Memory State where
    output p s = memory s

instance Numerical Memory where
    encode xs = make $ zip [ 1 .. ] xs
    decode m = get m 0


