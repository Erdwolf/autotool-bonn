module Read 

( parsed
)

where

import Ids
import Syntax


import Ops (ops)

import Defaults
import Semantik
import ExpParse

import FiniteMap

-------------------------------------------------------------



hack [] = []
hack ("" : css) = hack css
hack (cs : css) =
    let (dss, ess) = 
	      span (\ cs -> null cs || head cs `elem` " \t") css
    in
	concat (cs : dss) : hack ess

unco ('-' : '-' : rest ) = ""
unco (c : cs) = c : unco cs
unco "" = ""

exen oi [] = []
exen oi (cs : css) =
     case pline oi cs of
	  (Just x , oi') -> (oi', x) : exen oi' css
	  (Nothing, oi') ->            exen oi' css

exes oi css =
    let oixs = exen oi css
    in  (fst (last oixs), map snd oixs)


parsed :: String -> [ Exp ]
parsed inp =	 
    let
	(oi, xs) = exes (opts0, ops) . hack . map unco . lines $ inp
    in
	xs

