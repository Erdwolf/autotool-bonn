module RAM.Machine where

-- $Id$

import Machine.Class

import RAM.Type
import RAM.Memory
import RAM.State
import RAM.Step

import Set
import Size

instance Compute Program State where
    next p s = mkSet $ step s
    accepting p s = null $ todo s

instance InOut Program Memory State where
    input  p m = State { memory = m, todo = p, schritt = 0, past = [] }
    output p s = memory s

instance Numerical Memory where
    encode xs = make $ do
        ( k, x ) <- zip [ 1 .. ] xs
	return ( "x" ++ show k , x )
    decode m = get m "x0"


