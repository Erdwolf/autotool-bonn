module Robots.Generator where

--  $Id$

import Robots.Config
import Robots.Data
import Robots.Solver
import Robots.Nice

import Autolib.Util.Splits
import Autolib.ToDoc

import Challenger
import Data.List ( intersperse )

import Random
import IO
import IORef

-- | erzeugt eine konfiguration mit n robots,
-- ohne ziele, alle im bereich (-w, -w) .. (+w, +w)
some :: Int -> Integer -> IO Config
some n w = do
    let punkt = do 
	    x <- randomRIO (-w, w) ; y <- randomRIO (-w, w); return (x,y)
    let rob c = do
	    p <- punkt
	    return $ Robot { name = [c] , position = p, ziel = Nothing }
    rs <- mapM rob $ take n [ 'A' .. ]
    return $ make rs

form :: Config -> [ Zug ] -> Einsendung Robots Config [ Zug ]
form k zs = 
    Aufgabe { problem = Robots
	    , instanz = k
	    , beweis = zs
	    }

-- | nimmt eine konfiguration und setzt bei einem roboter
-- das ziel auf (0,0)
somes :: Config -> [ Config ]
somes k = do
    ( pre, x : post ) <- splits $ robots k
    return $ make $ pre ++ [ x { ziel = Just (0,0) } ] ++ post

----------------------------------------------------------------------------

action :: IORef Int -> Int -> Integer -> IO ()
action top n w = do
    k0 <- some n w
    sequence_ $ do
        k <- somes k0
        return $ do
	    -- print k
            t <- readIORef top
	    case shortest k of
	        zs : _ | length zs >= t -> do 
                    let l = length zs
                    writeIORef top l
		    let d = vcat [ toDoc l
				 , toDoc $ form k zs
				 , nice k
				 ]
		    print d
                    let fname = concat 
			      $ intersperse "-" 
			      [ "robots", show n, show w, show l ]
		    appendFile fname $ show d
		_	-> putStr "*"
	    hFlush stdout


