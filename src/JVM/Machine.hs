module JVM.Machine where

--   $Id$

import Machine.Class

import JVM.Type
import JVM.Memory
import JVM.State
import JVM.Step

import Autolib.Set
import Autolib.Size
import Data.Array

instance Compute Program State where
    next p s = mkSet $ step s
    accepting p s =   inRange ( bounds $ code s ) ( pc s )
		  &&  Halt == code s ! pc s
    depth p s = schritt s

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

instance Encode Memory where
    -- put argument list in x1, x2, ...
    encode xs = make $ zip (map Var [ 1 .. ]) xs

instance Decode Memory where
    -- get result from x0
    decode m = get m (Var 0)


