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

import Control.Monad ( when, guard ) 
import System.Environment
import Data.IORef
import Data.Ix

import System.IO

main :: IO ()
main = do
    [ n, w ] <- getArgs
    top <- newIORef 0
    sequence_ $ repeat $ action1 top ( read n ) ( read w )

action1 top n w = do
    let border = do
	    p @ (x,y) <- range ((-w,-w),(w,w))
--	    guard $ abs x > w - 2 || abs y > w - 2
	    guard $ abs x > w - 3 && abs y > w - 3
	    return p
    mid0 <- some_without_target n border
    let mid = attach_target_to_first (0,0) mid0
    handle top [ mid ]

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
