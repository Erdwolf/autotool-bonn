module Sortier.Netz.Rechnung where

-- $Id$

import Sortier.Netz.Type
import Sortier.Netz.Example

rechnung :: Netz -> State -> States
rechnung n ins = helper (comps n) ins where


helper :: Comps -> State -> States
helper xys state = state :
        case xys of 
             [] -> []
	     xy : rest -> helper rest (step xy state) 

step :: Comp -> State -> State
step (x1,y1) state =
    let x = pred x1 ; y = pred y1
    in	if state !! x > state !! y
	then state `with` [ (x, state !! y), (y, state !! x) ]
	else state

with :: [a] -> [(Int, a)] -> [a]
with l xys = foldl ( \ l (x, y) ->
	     let (pre, this : post) = splitAt x l
	     in  pre ++ y : post
	   ) l xys

