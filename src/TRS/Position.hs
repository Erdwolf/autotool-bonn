module TRS.Position where

-- $Id$

import TRS.Term

import Monad ( guard )
import Maybe ( fromMaybe )

type Position = [ Int ]

positions :: Term a -> [ Position ]
positions ( Node x cs ) = [] : do
    (p, c) <- zip [ 0 .. ] cs
    ps <- positions c
    return $ p : ps

mpeek :: Term a -> Position -> Maybe ( Term a )
mpeek t [] = return t
mpeek ( Node x cs ) ( p : ps ) = do
    guard $ 0 <= p && p < length cs
    mpeek (cs !! p) ps

peek :: Term a -> Position -> Term a
peek t ps = fromMaybe ( error "TRS.Position.peek" )
	  $ mpeek t ps

poke :: Term a -> ( Position, Term a ) -> Term a
poke t ( [], s ) = s
poke ( Node x cs ) ( p : ps, s ) = 
    let ( pre, c : post ) = splitAt p cs
    in  Node x ( pre ++ poke c (ps, s) : post )

