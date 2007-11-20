module String_Matching.BM.Semantics where

import String_Matching.Option
import String_Matching.BM.Instance

import Autolib.Util.Splits
import Autolib.Util.Zufall
import Autolib.FiniteMap
import Autolib.Set

import Data.List 
import Control.Monad ( guard )

-------------------------------------------------------------------------

bad_char :: Ord a 
	 => [a] -> [a] -> [ Int ]
bad_char sigma w = do
    let fm0 = listToFM $ zip sigma $ repeat 0
        update fm (x,k) = addToFM fm x k
        fm = foldl update fm0 $ zip w [ 1 .. ]
    x <- sigma
    return $ the $ lookupFM fm x

the ( Just x ) = x

similar u v = isSuffixOf u v || isSuffixOf v u

good_suff :: Eq a => [a] -> [ Int ]
good_suff w = do
    k <- [ 1 .. length w ]
    return $ longest_match w k

longest_match :: Eq a => [a] -> Int -> Int
longest_match w k = maximum $ do
    let suff = drop k w
    pre <- inits w
    guard $ not $ pre == w
    guard $ similar pre suff
    return $ length pre

-------------------------------------------------------------------------

start :: Ord a => [a] -> [a] -> Instance a
start sigma w = 
    Instance 
              { word = map Yes w
              , bad_character = map Yes $ bad_char sigma w
	      , good_suffix = map Yes $ good_suff w
              , alphabet = sigma
              }

all_deletions :: Ord  a => Instance a -> [ Instance a ]
all_deletions b = 
    do ( pre, Yes c : post ) <- splits $ word b 
       guard $ yes ( pre ++ post ) > 0
       return $ b { word = pre ++ No : post }
 ++ do ( pre, Yes c : post ) <- splits $ bad_character b 
       guard $ yes ( pre ++ post ) > 0
       return $ b { bad_character = pre ++ No : post }
 ++ do ( pre, Yes c : post ) <- splits $ good_suffix b 
       guard $ yes ( pre ++ post ) > 0
       return $ b { good_suffix = pre ++ No : post }


next :: Ord a => Instance a -> [ Instance a ]
next b = do
    c <- all_deletions b
    guard $ is_unique c
    return c

----------------------------------------------------------------------------

reduce :: ( RandomC m, Ord a )
       => [a]
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
    [ w ] -> True
    _   -> False

reconstruct :: Ord a 
            => Instance a
            -> [[a]]
reconstruct b = do
    w <- candidates ( alphabet b ) ( word b )
    guard $ sub ( bad_character b ) ( map Yes $ bad_char ( alphabet b ) w )
    guard $ sub ( good_suffix b ) ( map Yes $ good_suff w )
    return w

candidates sigma [] = return []
candidates sigma (p : ps) = do
     xs <- candidates sigma ps
     x <- sigma
     guard $ sub p $ Yes x
     return $ x : xs





