{-# OPTIONS -fallow-overlapping-instances #-}

--  $Id$

module PCP.Compressed where

import PCP.Type
import PCP.DFS

import Autolib.Schichten
import Autolib.Util.Splits
import Autolib.Sets
import Autolib.ToDoc
import Autolib.Util.Hide

import Control.Monad

m :: Int
m = 3

down :: Int -> Int
down x = x `mod` m

type SRS c = [([c],[c])]

inverse :: SRS c -> SRS c
inverse srs = do
    (l, r) <- srs
    return (r, l)

expand :: [Int] 
       -> SRS Int
expand w = do
    let (l, r) = ([0], map succ w)
    d <- [0 .. pred m]
    return ( map (down . (d + )) l
	   , map (down . (d + )) r
	   )



next :: Ord c
     => SRS c -> ([c], Hide[[c]]) -> Set( [c], Hide [[c]] )
next srs (w, Hide hist) = mkSet $ do
    (pre, midpost) <- splits w
    (l, r) <- srs
    let (mid, post) = splitAt (length l) midpost
    guard $ mid == l
    return ( pre ++ r ++ post, Hide $ w : hist )



test w = layers (expand w) (init w) (tail w) 
{-
layers :: SRS Int -- ^ expanding
       -> [Int] -- ^ top
       -> [Int] -- ^ bot
       -> [(Int,[Int],[Int])]
-}
layers srs top bot = do
    let tops = schichten (next srs) (top, Hide [])
        bots = schichten (next srs) (bot, Hide [])
    (k, (t, b)) <- zip [0..] $ zip tops bots
    let b' = mkSet $ do
           (w', Hide hw') <- setToList b
	   w'' <- equivs w'
           return (w'', Hide $ w' : hw')
    return (k, intersect t b')

topdown conf = do
    let start = init conf ; end = tail conf
    w <- fore conf start
    w' <- equivs w
    (w'', Hide hist) <- bfs -- dfs 
	   ( next ( inverse $ expand conf)) (w', Hide [] )
    guard $ w'' == end
    (w0, Hide fute) <-  bfs -- dfs 
	   ( next ( inverse $ expand conf)) (w, Hide [] )
    guard $ w0 == start
    return ( start : fute, reverse $ w'' : hist)

-- | compute successore more effectively
fore w this = do
    t <- [0..]
    let handle xs  0 = return xs
	handle (x : xs) lr = do
                let x' = map (down . succ . (x+)) w
                l <- if null xs then [ lr ] else [0 .. lr ]
                let r = lr - l 
                pre  <- if l == 0 then [[x]] else handle x' (l - 1)
                post <- handle xs r
                return $ pre ++ post
    handle this t

bruteforce w d = do
    let srs = expand w ++ inverse (expand w)
        nach w = mkSet $ do
             guard $ length w <= d
             (w', Hide hist) <- setToList $ next srs (w, Hide [])
             equivs w'
    t <- bfs nach (init w) 
    -- guard $ t == tail w
    return t

equivs :: [Int] -> [[Int]]
equivs w = do
    (u, v) <- splits w
    let vu = v ++ map (down . pred) u
    d <- [ 0 .. pred m ]
    return $ map (down . (d+)) vu


equiv :: [Int ] -> [Int ] -> Bool
equiv w w' = or $ do
    guard $ length w == length w'
    (u, v) <- splits w
    let vu = v ++ map (down . pred) u
    let ds = zipWith (-) vu w'
    return $ 1 == cardinality (mkSet ds)

