module Binpack.Example where

import Binpack.Instance

e1 :: Instance
e1 = Instance
   { weights = [ 9,1,4,3,6,2,8,4,6,5,7,3,7,5 ]
   , capacity = 10
   , bins = 7
   }
