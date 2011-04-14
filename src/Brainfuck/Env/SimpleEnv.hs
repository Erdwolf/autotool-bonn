{-# LANGUAGE DeriveDataTypeable #-}


module Brainfuck.Env.SimpleEnv where 

import qualified Data.List
import Brainfuck.Syntax

import Machine.History


import Autolib.ToDoc

import Data.Typeable

data Env =
    Env { left :: [ Integer ]
    	, right :: [ Integer ]
	, input :: [ Integer ]
	, output :: [ Integer ]
	, cmd :: [ Statement ]
	, steps :: Integer 
	, previous :: [ Env ]
	}
    deriving (Show, Ord, Eq, Typeable)

instance ToDoc Env where 
    toDoc e = text "Env" <+> vcat [
       text "band" <+> equals	<+> toDoc ( reverse ( left e ) ) <+> toDoc (head (right e)) <+> toDoc (tail (right e))
       , text "input" <+> equals <+> toDoc ( input e )
       , text "output" <+> equals <+> toDoc ( output e )
       , text "cmd" <+> equals <+> toDoc ( cmd e )
       , text "steps" <+> equals <+> toDoc ( steps e )
       ]
 
    

    
instance History Env where
    history e = previous e
    

newEnv :: [ Integer ] -> [ Statement ] -> Env
newEnv i c = Env { left = [], right = [0], input = i, output = [], cmd = c, steps = 0, previous = [] }

inc :: Env -> Env
inc (Env l (x : xs) i o c s p) = Env {left = l, right = [x+1] ++ xs, input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l (x:xs) i o c s p) ] ++ p}
       
dec :: Env -> Env
dec (Env l (x : xs) i o c s p)  = Env {left = l, right = [x-1] ++ xs, input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l (x:xs) i o c s p) ] ++ p}


shift_r :: Env -> Env
shift_r (Env l (x : xs) i o c s p) = case xs of
                          [] -> Env {left = [x] ++ l, right = [0], input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l (x:xs) i o c s p) ] ++ p}
                          _  -> Env {left = [x] ++ l, right = xs, input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l (x:xs) i o c s p) ] ++ p}



shift_l :: Env -> Env
shift_l (Env l r i o c s p) = case l of
                  [] -> Env {left = [], right = [0] ++ r, input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l r i o c s p) ] ++ p}
                  (x : xs) -> Env {left = xs, right = [x] ++ r, input = i, output = o, cmd = tail c, steps = s+1, previous = [ (Env l r i o c s p) ] ++ p}

get :: Env -> Env
get (Env l r i o c s p) = Env { left = l, right = r, input = i, output = o ++ [head r], cmd = tail c, steps = s+1 , previous = [ (Env l r i o c s p) ] ++ p} 

set :: Env -> Env
set (Env l (x : xs) (ix : ixs) o c s p)  = Env {left = l, right = [ix] ++ xs, input = ixs, output = o, cmd = tail c, steps = s+1, previous = [ (Env l (x:xs) (ix : ixs) o c s p) ] ++ p}

next :: Env -> Env
next (Env l r i o c s p) = case head c of
    Block sts -> Env { left = l, right = r, input = i, output = o, cmd = sts, steps = s, previous = p }
    Plus -> inc ( (Env l r i o c s p) )    
    Minus -> dec ( (Env l r i o c s p) )
    Output -> get ( (Env l r i o c s p) )
    Input -> set ( (Env l r i o c s p) )
    MRight -> shift_r ( (Env l r i o c s p) )
    MLeft -> shift_l ( (Env l r i o c s p) )
    Loop sts -> if ( head r /= 0 ) 
                then Env { left = l, right = r, input = i, output = o, cmd = sts ++ c, steps = s+1, previous = [ (Env l r i o c s p) ] ++ p}
                else Env { left = l, right = r, input = i, output = o, cmd = tail c, steps = s +1, previous = [ (Env l r i o c s p) ] ++ p}
--    _ -> error $ "Eval.statement: " ++ show head c    

accepting :: Env -> Bool
accepting (Env l r i o c s p) = case c of
    [] -> True
    _ -> False
    
depth :: Env -> Integer
depth (Env l r i o c s p) = s
    
