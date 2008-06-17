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

vars ks = do k <- ks ; return $ Variable k

e2 :: CNF
e2 = equiv $ vars [ 1 .. 5 ]

equiv   vs = CNF $ map Clause $ fun True  vs
inequiv vs = CNF $ map Clause $ fun False vs

fun True  [] = return []
fun False [] = []
fun p (v : vs) = 
             map ( Literal v True  : ) ( fun p vs )
          ++ map ( Literal v False : ) ( fun (not p) vs )

merge :: [ CNF ] -> CNF
merge cnfs = CNF $ concat $ map ( \ (CNF cs) -> cs ) cnfs

e2better :: CNF
e2better = merge [ inequiv $ vars [ 1,2,6 ]
                 , inequiv $ vars [ 3,4,7 ]
                 , equiv $ vars [5,6,7]
                 ]
    



