module RAM.Machine where

-- $Id$

import Machine.Class

import RAM.Type
import RAM.Memory
import RAM.State
import RAM.Step

import Set
import Size


instance InOut Program Memory State where
    input  p m = State { memory = m, todo = p }
    output p s = memory s

instance Numerical Memory where
    encode xs = make $ do
        ( k, x ) <- zip [ 1 .. ] xs
	return ( "x" ++ show k , x )
    decode m = get m "x0"

instance Compute Program State where
    next m s = mkSet $ step s
    accepting m s = null $ todo s -- ??

