module Robots.Generator where

import Robots.Type
import Robots.Solver
import Robots.Nice

import Challenger

import Random
import IO

some :: Int -> Integer -> IO Konfig
-- erzeugt eine konfiguration mit n robots,
-- der erste hat ziel (0,0), die anderen haben keine ziele
-- alle im bereich (-w, -w) .. (+w, +w)
some n w = do
    let punkt = do 
	    x <- randomRIO (-w, w) ; y <- randomRIO (-w, w); return (x,y)
    let rob c = do
	    p <- punkt
	    return $ Robot { name = [c] , position = p, ziel = Nothing }
    r : rs <- mapM rob $ take n [ 'A' .. ]
    let z = ( 0, 0 )
    return $ mkKonfig ( r { ziel = Just z } : rs )

form :: Konfig -> [ Zug ] -> Einsendung Robots Konfig [ Zug ]
form k zs = 
    Aufgabe { problem = Robots
	    , instanz = k
	    , beweis = zs
	    }

action :: Int -> Integer -> IO ()
action n w = do
    k <- some n w
    -- print k
    case shortest k of
	 zs : _ | length zs > 5 -> do 
	    putStrLn $ "\nSOL: " ++ show (length zs)
	    print $ form k zs
	    print $ nice k
	 _	-> putStr "*"
    hFlush stdout


