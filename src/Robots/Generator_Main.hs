module Main (main) where

import Robots.Nice
import Robots.Generator

import Autolib.ToDoc

import System.Environment
import Data.IORef
import Data.Ix


main :: IO ()
main = do
    [ n, w ] <- getArgs
    top <- newIORef 0
    sequence_ $ repeat $ action top ( read n ) ( read w )

action top n w = do
    ( i, zss ) <- sol 10000 n $ range ((-w,-w),(w,w))
    best <- readIORef top
    case zss of
	( zs : _ ) | length zs >= best -> do
	    print $ nice i
	    print i
	    print zs
	    print $ length zs
	    writeIORef top $ length zs
	_ -> return ()
