module CNF.Example where

import CNF.Form

e1 :: CNF
e1 = read 
   $ unlines [ "(-x1 + -x2 + x8) * (-x1 + -x4 + x8) * (-x1 + x6 + x9) *"
             , "(-x1 + x7) * (x1 + x4 + -x7) * (x1 + x5 + -x8) * (x1 + x6 + -x9) *"
             , "(-x2 + -x3 + -x6 + x9) * (-x2 + -x5 + x8) * (x2 + -x3 + -x5 + x9) *"
             , "(x2 + x4 + -x8) * (x2 + x5 + x7) * (x2 + x5 + -x9) * (x2 + x6) *"
             , "(x3 + -x4 + x9) * (x3 + x4 + -x9) * (x3 + x5) * (x3 + x6) *"
             , "(-x4 + -x5 + x8) * (-x4 + x7)"
             ]

