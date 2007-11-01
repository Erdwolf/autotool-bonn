module Robots.Scheme where

import Robots.Nice
import Robots.Generator
import Robots.Solver
import Robots.Config
import Robots.Data
import Robots.Move

import Autolib.ToDoc
import Autolib.Set
import Autolib.Schichten
import Autolib.Util.Zufall

import Control.Monad ( when, guard ) 
import System.Environment
import Data.IORef
import Data.Ix

import System.IO

corners d w = do
    p @ (x,y) <- range ((-w,-w),(w,w))
    guard $ abs x > w - d && abs y > w - d
    return p

at_corners off k = and $ do
    let ((a,b),(c,d)) = bounds k
    r <- robots k
    let (x,y) = position r
    return $  ( x < a + off || x > c - off )
           && ( y < b + off || y > d - off )
    

cornered w k = only_corners w k && each_corner w k

each_corner w k = and $ do
    let ps = do r <- robots k ; return $ position r
    fx <- [ -1, 1 ]
    fy <- [ -1, 1 ]
    return $ ( 2 == ) $ length $ do
        (x,y) <- ps
        guard $ x*fx > w && y*fy > w
        return ()

only_corners w k = and $ do
    r <- robots k
    let (x,y) = position r
    return $ abs x >= w && abs y >= w

bounds k = 
    let ps = do r <- robots k ; return $ position r
        xs = map fst ps ; ys = map snd ps
    in  ( ( minimum xs, minimum ys )
        , ( maximum xs, maximum ys )
        )

action n w = do
    start0 <- ( some n $ corners 2 w )
           `repeat_until` \ k -> at_corners 2 k
    let start = make $ do 
            r <- robots start0 
            return $ r { ziel = Nothing }
    -- print start
    -- print $ nice start
    let b = bounds start
        kss = id -- take 20
           $ map setToList
           $ schichten ( mkSet . nachfolger_without_loss ) start
    print ( map length kss )
    sequence_ $ do
        ks <- kss
        k <- ks
--        guard $ n == length ( robots k )
--        guard $ at_corners 2 k -- cornered (w-3) k 
--        guard $ four_corners k
        let bk = bounds k
--        guard $ contains b bk
        guard $ extension bk + 2 < extension b
        -- guard $ b /= bk
        return $ do
            let form k = vcat [ toDoc (  bounds k ) , nice k ]
            print $ besides [ form start, text "=>", form k ]

extension ((a,b),(c,d)) = maximum $ map abs [a,b,c,d]

contains ((a,b),(c,d)) ((p,q),(r,s)) =
   ( 4 <= ) $ length $ filter id [ a < p , b < q, c > r, d > s ]


