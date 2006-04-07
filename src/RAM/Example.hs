module RAM.Example where

import RAM
import Autolib.TES.Identifier

student :: Program
student = 
    let x0 = mkunary "x0"
        x1 = mkunary "x1"
    in  [ Inc x1
        , Loop x1 [ Loop x1 [ Inc x0 ] ]
        ]
