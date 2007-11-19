module String_Matching.KMP.Semantics where

import String_Matching.Option
import String_Matching.KMP.Instance

import Autolib.Util.Splits
import Autolib.Util.Zufall
import Autolib.Set

import Data.List
import Control.Monad ( guard )


-------------------------------------------------------------------------

failure :: Ord a =>  [a] -> [Int]
failure w = do
    u <- inits w
    guard $ not $ null u
    return $ self_overlap u

self_overlap u= maximum $ do
        pre <- inits u
        guard $ not $ pre == u
        guard $ pre `isSuffixOf` u
        return $ length pre

-------------------------------------------------------------------------

start :: Ord a => Set a -> [a] -> Instance a
start s w = Instance 
              { word = map Yes w
              , failures = map Yes $ failure w 
              , alphabet = s
              }

all_deletions :: Ord  a => Instance a -> [ Instance a ]
all_deletions b = 
    do ( pre, Yes c : post ) <- splits $ word b 
       return $ b { word = pre ++ No : post }
 ++ do ( pre, Yes c : post ) <- splits $ failures b 
       return $ b { failures = pre ++ No : post }

next :: Ord a => Instance a -> [ Instance a ]
next b = do
    c <- all_deletions b
    guard $ is_unique c
    return c

----------------------------------------------------------------------------

reduce :: ( RandomC m, Ord a )
       => Set a
       -> [a] 
       -> m ( Instance a )
reduce sigma w = do
    let f b = case next b of
                [] -> return b
                bs -> eins bs >>= f
    f $ start sigma w

-------------------------------------------------------------------------

is_unique :: Ord a => Instance a -> Bool
is_unique b = case reconstruct b of
    [w] -> True
    _   -> False

reconstruct :: Ord a 
            => Instance a
            -> [[a]]
reconstruct b =
    map reverse $ reconstruct_helper ( alphabet b )
                $ reverse $ zip ( word b ) ( failures b )

reconstruct_helper :: Ord a
            => Set a
            -> [ ( Option a, Option Int) ] -- ^ zipped and in reverse order
            -> [[a]] -- ^  in reverse order
reconstruct_helper sigma [] = return  []
reconstruct_helper sigma ((c,n) : cns) = do
    w <- reconstruct_helper sigma cns
    x <- setToList sigma
    guard $ c `sub` Yes x
    let w' = x : w
    let s = self_overlap w'
    guard $ n `sub` Yes s
    return w'

