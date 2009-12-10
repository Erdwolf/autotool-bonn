--  $Id$

module PCP.Compressed where

import PCP.Type
import PCP.DFS
import PCP.Form

import PCP.Reach

import Autolib.Schichten
import Autolib.Util.Splits
import Autolib.Util.Sort
import Autolib.Set
import Autolib.ToDoc
import Autolib.Util.Hide
import Autolib.Util.Wort

import Control.Monad
import Data.Ratio
import IO

m :: Int
m = 3

down :: Int -> Int
down x = x `mod` m



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
     => SRS c 
     -> ( [c] -> Bool ) -- ^ prefix ok?
     -> ([c], Hide[[c]]) 
     -> Set( [c], Hide [[c]] )
next srs ok (w, Hide hist) = mkSet $ do
    (pre, midpost) <- splits w
    guard $ ok pre
    (l, r) <- srs
    let (mid, post) = splitAt (length l) midpost
    guard $ mid == l
    return ( pre ++ r ++ post, Hide $ w : hist )

leftmost_next :: Ord c
    =>  SRS c
    -> ( [c] -> Bool )
    -> ([c], [c], Hide [[c]])
    -> Set([c], [c], Hide [[c]] )
leftmost_next srs ok (done, todo, Hide hist) = mkSet $ do
    let w = done ++ todo
    (pre, midpost) <- splits todo
    guard $ ok $ done ++ pre
    (l, r) <- srs
    let (mid, post) = splitAt (length l) midpost
    guard $ mid == l
    return ( done ++ pre, r ++ post, Hide $ w : hist )

leftmost srs ok start = do
    (here, there, h) <- bfs (leftmost_next srs ok) ([], start, Hide [])
    return ( here ++ there, h )
    
runit :: IO ()
runit = sequence_ $ do
    n <- [0 .. ]
    w' <- alle [0, 1] n
    let w = [0] ++ w' ++ [1]
	spiegel = reverse . map ( 1 - )
    guard $ w <= spiegel w
    return $ handle w

handle w = do
     line
     pft ("parameter", w)
     let f = concat $ map show w
     pft ("instance", form f)
     blank
     let ntd @ (n, t, d) : _ = topdown w
     pft $ ntd
     let l = length w - 1
     print ("form", look tops $ analyze l t, look bots $ analyze l d, f)
     line


printf x = do print x ; hFlush stdout

pft :: ToDoc a => a -> IO ()
pft = printf . toDoc

line = putStrLn $ "*****************************************"
blank = putStrLn ""

topdown conf = do
    let start = init conf ; end = tail conf
    let srs = inverse $ expand conf
        -- restrict derivations
        ok pre = ( length pre `mod` (length conf - 1) ) <= 1
    (w, Hide future) <- leftmost (expand conf) ok start
    w' <- equivs w
 
    let ns = numbers srs (w', end)

    guard $ and $ do
         n <- ns ; return $ 1 == denominator n
    guard $ and $ do
         n <- ns ; return $ 0 <= numerator n

    let ok pre = 0 == (1 + length pre) `mod` (length conf - 1)
    (w'', Hide hist) <- bfs ( next srs (const True) ) (w', Hide [] )
    guard $ w'' == end

    return ( ns, reverse $ w : future, reverse $ w'' : hist)

-- TODO: should go into ToDoc/*
instance (Integral a, ToDoc a) => ToDoc (Ratio a) where
    toDoc r = fsep [ toDoc (numerator r), text "%", toDoc (denominator r) ]

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
             (w', Hide hist) <- setToList $ next srs (const True) (w, Hide [])
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

-------------------------------------------------------------------------

common :: Eq c => [c] -> [c] -> Int
common (x : xs) (y : ys) | x == y = succ $ common xs ys
common _ _ = 0

analyze :: Int -> [[Int]] -> [(Int,Int)]
analyze l ws = do
    (u, v) <- zip ws (tail ws)
    let c = common u v
    let (d, m) = divMod c l
    return ( d, if m > 1 then m - l else m )

look :: Eq a
     => [a] -> a -> Either a Int
look ys x = 
    case do (i, y) <- zip [0..] ys ; guard $ x == y ; return i of
        [i] -> Right i
        _   -> Left x

tops = [ [(0,0),(0,0),(0,1),(0,1),(1,1),(2,1)] -- 0
       , [(0,0),(0,0),(0,1),(1,0),(3,0),(4,0)] -- 1
       , [(0,0),(0,0),(0,1),(1,1),(2,1)]       -- 2
       , [(0,0),(0,0),(0,1),(1,1)]             -- 3
       , []                                    -- 4
       , [(0,0),(0,0),(0,1),(1,1),(2,1),(3,0)] -- 5
       , [(0,1),(0,1),(1,1),(2,1)]             -- 6
       ]

bots = [ [(1,1),(1,1),(1,1),(0,1)]                   -- 0
       , [(2,-1),(2,-2),(2,-2),(2,-2),(1,-1),(0,-1)] -- 1
       , [(2,-2),(2,-2),(2,-2),(1,-1),(0,-1)]        -- 2
       , [(2,-2),(2,-2),(2,-2),(1,-2),(0,-2),(0,-2)] -- 3
       , [(2,1),(2,1),(2,1),(1,1),(0,1),(0,1)]       -- 4
       , []                                          -- 5
       , [(3,-2),(3,-2),(3,-2),(2,-2),(1,-1),(0,-1)] -- 6
       , [(2,0),(2,0),(1,1),(0,1)]                   -- 7
       ]

-- patterns occuring: 
--   (2/2) for  00x11  and 01x01
--   (0,1) for  00x01  (iso. to 01x11 ?)

