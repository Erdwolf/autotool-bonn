module Fun.Machine where

--   $Id$

import Machine.Class

import Fun.Type
import Fun.State
import Fun.Step

import qualified Fun.Cache

import Data.Set
import Size

instance Compute Fun State where
    next p s = mkSet $ step s
    accepting p s = null $ step s -- ??

instance InOut Fun [ Integer ] State where
    input  p m = State { todo = [App p $ map Zahl m]
		       , stack = []
		       , schritt = 0
		       , past = [] 
		       , cache = Fun.Cache.empty
		       }
    output p s = stack s

instance Numerical [ Integer ] where
    encode = id
    decode [x] = x



