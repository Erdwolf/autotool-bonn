module JVM.Step where

-- $Id$

import JVM.Type
import JVM.Memory
import JVM.State

import Data.Array
import Control.Monad ( guard )

step :: State -> [ State ]
-- liste aller direkten nachfolge-zust‰nde
-- for deterministische maschinen hier immer l‰nge <= 1
step s = do
    guard $ inRange (bounds $ code s) (pc s)
    let b = code s ! pc s ; s' = stepped s
    case b of
         Push i -> push i s'
	 Drop   -> JVM.Step.drop s' 
	 Dup    -> dup s'
         Add    -> zwei (+) s' ; Sub    -> zwei (-) s' ; Mul    -> zwei (*) s'
         Load   -> load s'     ; Store  -> store s'
	 Jump d -> jump d s'   ; Jumpz d -> jumpz d s'
	 Halt   -> [ ]

-----------------------------------------------------------------------

stepped :: State -> State
-- normalerweise gehts einen schritt weiter
stepped s =  s { schritt = succ $ schritt s
	       , pc = succ $ pc s
	       , past = s : past s
	       }
	    
-----------------------------------------------------------------------

push :: Integer -> State 
     -> [ State ]
push i s = do
	    return $ s { stack = i : stack s }

pop :: State
    -> [ ( Integer, State ) ]
pop s = do
	    -- paﬂt nur, falls nicht leer
	    top : rest <- return $ stack s
	    return ( top , s { stack = rest } )

drop :: State -> [ State ]
drop s = do ( _ , s ) <- pop s ; return s

dup :: State -> [ State ]
dup s = do 
    ( a, s ) <- pop s
    s <- push a s
    s <- push a s
    return s

-----------------------------------------------------------------------

zwei :: ( Integer -> Integer -> Integer )
     -> State
     -> [ State ]
zwei fun s = do
    ( b , s ) <- pop s
    ( a , s ) <- pop s
    push ( fun a b ) s

-----------------------------------------------------------------------

load :: State -> [ State ]
load s = do
    (a, s) <- pop s
    push ( get (memory s) a ) s

store :: State -> [ State ]
store s = do
    (a, s) <- pop s
    (b, s) <- pop s
    return $ s { memory = set ( memory s ) ( a, b ) }

-----------------------------------------------------------------------

jump :: Int -> State -> [ State ]
jump d s = return $ s { pc = pc s + d }

jumpz :: Int -> State -> [ State ]
jumpz d s = do
    ( a, s ) <- pop s
    if 0 == a 
       then jump d s
       else return s

