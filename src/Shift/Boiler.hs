module Shift.Boiler where


import Shift.Computer ( folge, smp )
import Shift.Linear
import Shift.Repeater hiding ( repeater )

import List ( group )
import ToDoc


ak k = folge [2*k, 3*k, 5*k+1, 7*k+1]
ex k = map Item 
   $ map ( \ x -> if x then '+' else '-' ) 
   $ ak k

