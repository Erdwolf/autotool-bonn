module RAM.Machine where

--   $Id$

import Machine.Class

import RAM.Type
import RAM.Memory
import RAM.State
import RAM.Step

import Data.Set
import Size

instance Compute Program State where
    depth _ = schritt
    next p s = mkSet $ step s
    accepting p s = null $ todo s

instance In Program Memory State where
    input  p m = State { memory = m, todo = p, schritt = 0, past = [] }
instance Out Program Memory State where
    output p s = memory s

instance Encode Memory where
    encode xs = make $ do
        ( k, x ) <- zip [ 1 .. ] xs
	return ( "x" ++ show k , x )
instance Decode Memory where
    decode m = get m "x0"


