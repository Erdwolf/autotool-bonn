module Hilbert.Sub

( subcons
, subs
)

where

-- import Syntax
import Autolib.Set

subcons :: Exp -> [ (Exp, Exp -> Exp) ]
subcons x @ (App fun args) = (x, id) :
	[ (t, (App fun) . c) | (t, c) <- subconlist args ]

subconlist :: [ Exp ] -> [ (Exp, Exp -> [Exp]) ]
subconlist [] = []
subconlist (x : xs) =
       [ (t, ( : xs) . c )   | (t, c) <- subcons x ]
   ++  [ (t, ( x : ) . c )   | (t, c) <- subconlist xs ]

subs :: Exp -> Set Exp
subs x = mkSet [ t | (t, c) <- subcons x ]
