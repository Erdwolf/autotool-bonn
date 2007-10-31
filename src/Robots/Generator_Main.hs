module Main (main) where

import Robots.Nice
import Robots.Generator
import Robots.Solver
import Robots.Config
import Robots.Data
import Robots.Move

import Autolib.ToDoc
import Autolib.Schichten
import Autolib.Util.Zufall ( eins )

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
    mid0 <- some n $ range ((-w,-w),(w,w))
    let rss = reachables mid0
    when ( False )
         $ hPutStr stderr $ "fore:" ++ show ( map length rss ) 

    far0 <- eins $ last $ reachables mid0

    let mid = repair mid0 far0

    let pss = predecessors mid
    when ( False )
         $ hPutStr stderr $ "back:" ++ show ( map length pss )

    -- hPutStr stderr "*"

    mapM_ ( handle top ) $ concat 
                         $ pss

repair k target = make $ do
    r <- robots k
    let z = do s <- look target ( name r )
               return $ position s
    return $ case ziel r of
               Nothing -> r
               Just _ -> r { ziel = z }

handle top i = do
    let zss = shortest  i
    best <- readIORef top
    case zss of
	( zs : _ ) | length zs >= best -> do
	    print i            
	    print $ nice i
	    print zs
	    putStrLn $ "length: " ++ show ( length zs )
	    hFlush stdout
	    writeIORef top $ length zs
	_ -> return ()
