{-# language PatternSignatures #-}

module Main (main) where

import Robots.Nice
import Robots.Generator
import Robots.Solver
import Robots.QSearch
import Robots.Config
import Robots.Data
import Robots.Move

import Autolib.ToDoc
import Autolib.Schichten
import Autolib.Util.Zufall ( eins, repeat_until )
import Autolib.Util.Sort 

import Control.Monad ( when, guard ) 
import System.Environment
import Data.IORef
import Data.Ix

import System.IO

main :: IO ()
main = do
    [ n, w ] <- getArgs
    top <- newIORef 0
    sequence_ $ repeat $ action2 top ( read n ) ( read w )


action2 top n w = do
    start0 <- some_without_target n $ border w
    let start = attach_target_to_first (0,0) start0
    print $ vcat [ nice start , text $ replicate 50 '*' ]
    let ks = find start
    mapM_ ( \ (b,c,zs) -> when ( is_shifted start c ) $ do
        print $ vcat [ besides [ nice start, nice c ]
				       , toDoc $ reverse zs 
				       ]
	error "huh"
      ) ks


find k = takeUntil ( \ (b,c,_) -> is_shifted k c )
       $ take 1000
       $ tail
       $ search znachfolger_all_onboard badness k

is_shifted k0 k1 = 
    let smp = sort . map position . robots
        diffs = zipWith (\ (a,b) (c,d) -> (a-c,b-d)) ( smp k0 ) ( smp k1 )
        tag (a,b) = if abs a == abs b then abs a else 0
        all_equal xs = 1 == length ( sort xs )
    in  k0 /= k1 && all_equal ( map tag diffs )

--------------------------------------------------------------------------

action1 top n w = do
    mid0 <- some_without_target n $ border w
    let mid = attach_target_to_first (0,0) mid0
    handle top [ mid ]

border w = do
	    p @ (x,y) <- range ((-w,-w),(w,w))
--	    guard $ abs x > w - 2 || abs y > w - 2
	    guard $ abs x > w - 3 && abs y > w - 3
	    return p

attach_target_to_first p k = make $ 
    let r : rs = robots k
    in  r { ziel = Just p } : rs

action0 top n w = do
    mid0 <- some_without_target n $ range ((-w,-w),(w,w))

    -- print $ vcat [ text "mid0", nice mid0 ]

    let fss :: [[Config]]
        fss = reachables mid0
    f :: Config <- eins ( last $ init fss ) 
           `repeat_until` \ f -> not $ null $ robots f
    r :: Robot <- eins $ robots f
    
    let mid :: Config
        mid = attach_target r mid0

    -- print $ vcat [ text "mid", nice mid ]

    ( b, c, zs ) <- qsolve mid

    let pss = [ mid ] : predecessors mid

    hPutStr stderr $ show ( length zs ) ++ "."
    best <- readIORef top
    mapM_ ( handle top ) 
         $ drop ( best - length zs )
         $ pss

attach_target r conf = make $ do
    s <- robots conf
    return $ if name r == name s
       then s { ziel = Just $ position r }
       else s

handle top is = do
    i <- eins is
    ( b, c, zs) <- qsolve i
    best <- readIORef top
    when ( ist_final c && length zs >= best ) $ do
            print i
	    print $ besides [ nice i , text "=>", nice c ]
	    print $ fsep 
		  $ map ( \ (n,d) -> text n <> text ":" <> toDoc d ) 
		  $ reverse zs
	    putStrLn $ "length: " ++ show ( length zs )
	    hFlush stdout
	    writeIORef top $ length zs

