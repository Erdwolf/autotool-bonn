module Main (main) where

import Robots.Nice
import Robots.Generator
import Robots.Solver
import Robots.Move

import Autolib.ToDoc
import Autolib.Schichten

import System.Environment
import Data.IORef
import Data.Ix

import System.IO

main :: IO ()
main = do
    [ n, w ] <- getArgs
    top <- newIORef 0
    sequence_ $ repeat $ action top ( read n ) ( read w )

action top n w = do
    i0 <- some n $ range ((-w,-w),(w,w))
    let pss = predecessors i0
    hPutStr stderr $ show ( map length pss )
    mapM_ ( handle top ) $ concat pss

handle top i = do
    let zss = shortest  i
    best <- readIORef top
    case zss of
	( zs : _ ) | length zs >= best -> do
	    print i            
	    print $ nice i
	    print zs
	    print $ length zs
	    hFlush stdout
	    writeIORef top $ length zs
	_ -> return ()
