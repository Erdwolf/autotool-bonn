module Shift.Boiler where

import Shift.Linear
import Shift.Computer
import Shift.Break


import Bits
import List ( group )
import ToDoc


ak k = [2*k,3*k,5*k+1,7*k+1]

ch f = if f then '+' else '-'

ex k = map ( \ g -> Repeat { start = [ Item $ ch $ head g ]
			   , diff  = [ DZero ]
			   , count = length g
			   }
	   ) $ group $ folge $ ak k

brk :: Break -> [ Linear () ]
brk ms = map ( \ n -> Repeat { start = [ Item () ], diff = [ DZero ]
			     , count = fromIntegral n
			     } )
       $ grundy ms

mainf a b c = smp $ worker False a $ take b $ brk c


