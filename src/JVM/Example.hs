module JVM.Example where

import JVM

student :: Program
-- soll berechnen: M[0] := M[1] * M[2]
student = 
    [ Push 1 , Load
    , Push 2 , Load
    , Add
    , Push 0 , Store
    , Halt
    ]
