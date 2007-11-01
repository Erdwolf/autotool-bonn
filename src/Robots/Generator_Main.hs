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

import Control.Monad ( when ) 
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
    when ( length zs >= best ) $ do
	    print i            
	    print $ nice i
	    print zs
	    putStrLn $ "length: " ++ show ( length zs )
	    hFlush stdout
	    writeIORef top $ length zs
