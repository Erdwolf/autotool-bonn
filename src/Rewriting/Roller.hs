{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.Roller where

import Rewriting.TRS

import Autolib.Util.Zufall
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

roll_trs :: ( Symbol v, Symbol c )
          => Config v c 
          -> IO ( TRS v c )
roll_trs conf = do
    rs <- sequence $ replicate ( num_rules conf ) $ roll_rule conf
    let vs = unionManySets $ do
             r <- rs
             [ vars $ lhs r, vars $ rhs r ]
    return $ TRS
           { variablen = setToList vs
           , regeln = rs
           }

roll_rule :: ( Symbol v, Symbol c )
          => Config v c 
          -> IO ( Rule ( Term v c ))
roll_rule conf = do
    num_lvars <- randomRIO ( 0, num_vars conf )
    lvars <- sequence $ replicate num_lvars $ eins $ variables conf
    num_rvars <- randomRIO ( 0, num_vars conf )
    rvars <- if 0 == num_lvars then return []
             else sequence $ replicate num_rvars $ eins lvars
    let sig = mksig $ signature conf
    ls <- randomRIO ( num_lvars + 1, max_size conf )
    lt <- roll_ground_term sig ls
    l <- put_vars lvars lt
    rs <- randomRIO ( num_rvars + 1, max_size conf ) 
    rt <- roll_ground_term sig rs
    r <- put_vars rvars rt
    return $ Rule { lhs = l, rhs = r, strict = True }

roll_ground_term :: Symbol c 
    => [ c ]
    -> Int -- ^ size
    -> IO ( Term v c )
roll_ground_term sig s | s <= 1 = do 
    f <- eins $ filter ( (== 0) . arity ) sig
    return $ Node f []
roll_ground_term sig s = do
    f <- eins $ filter ( ( > 0) . arity ) sig
    xs <- sequence $ do 
          k <- [ 1 .. arity f ]
          return $ do
              s <- randomRIO ( 1, pred s `div` arity f )
              roll_ground_term sig s
    return $ Node f xs


-- | put variables at random positions
put_vars vs t = do
    let leaves = leafpos t
    ps <- selektion ( length vs `min` length leaves ) $ leaves
    return $ pokes t $ zip ps $ map Var vs
    
-------------------------------------------------------------------------

data ( Symbol v, Symbol c ) => Config v c =
     Config { signature :: [ ( c, Int) ] -- ^ with arity
            , variables :: [ v ]
            , num_rules :: Int
            , max_size :: Int -- ^ of term in rules
            , num_vars :: Int -- ^ of variables in single rule
            }
    deriving ( Eq, Ord, Typeable )

example :: Config Identifier Identifier
example = Config
        { signature = read "[(a,0),(b,0),(f,1),(g,2)]"
        , variables = read "[X,Y,Z]"
        , num_rules = 3
        , max_size  = 8
        , num_vars  = 2
        }

mksig :: Symbol c => [(c,Int)] -> [c]
mksig cas = do ( c,a) <- cas ; return $ set_arity a c

{-! for Config derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end;
