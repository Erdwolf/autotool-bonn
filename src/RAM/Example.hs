module RAM.Example where

import RAM

ex :: Program
-- Wirkung:  x0 := 2 ^ x1
ex = [ Inc "x0"
     , Loop "x1" [ Loop "x0" [ Inc "x0" ] ]
     ]
