module Shift.Boiler where

import Shift.Type
import Shift.Generate ( pee )
import Shift.Meta ( vector )

import Shift.Computer ( smp )

m = Meta { start = [2,3,6,8], diff = [2,3,5,7] }
mm = do k <- [ 0 .. ] ; return $ pee $ vector m k


