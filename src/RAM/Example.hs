module RAM.Example where

import RAM

import LOOP.PRIM

student :: Program
-- Wirkung:  x0 := 2 ^ x1
student = [ Inc "x0"
     , Loop "x1" [ Loop "x0" [ Inc "x0" ] ]
     ]
