module RAM.Example where

import RAM
import RAM.Step

ex :: Program
ex = [ Loop "a" [ Dec "a" , Inc "c" ]
     , Loop "b" [ Dec "b" , Inc "c" ]
     ]
