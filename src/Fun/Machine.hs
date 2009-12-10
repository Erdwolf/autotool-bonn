module Fun.Machine where

--   $Id$

import Machine.Class

import Fun.Type
import Fun.State
import Fun.Step


import qualified Fun.Cache

import Autolib.Set
import Autolib.Size

instance Compute Fun State where
    next p s = mkSet $ step s
    accepting p s = null $ step s -- ??
    depth p s = schritt s

instance In Fun [ Integer ] State where
    input_reporter  p m = return $ input p m

input p m = State 
         { todo = [App p $ map Zahl m]
         , stack = []
         , schritt = 0
         , past = [] 
         , cache = Fun.Cache.empty
         }

instance Out Fun [ Integer ] State where
    output_reporter p s = return $ stack s

instance Encode [ Integer ] where
    encode = id

instance Decode [ Integer ] where
    decode [x] = x



