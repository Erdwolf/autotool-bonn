module Turing.Laufzeit where

-- $Id$

import Turing
import Turing.Akzeptieren
import Turing.Vorrechnen
import Turing.Konfiguration

import Monad (guard)
import Right
import Wrong
import Auswertung

test :: TUM Char z
     => (Int -> Int) -> [ Int ]
     -> Turing Char z 
     -> IO String

test f args m = do
    putStrLn $ "Ihre Turingmaschine ist"
    putStrLn $ show m

    muss (check m) $ do

	putStrLn $ "ich teste die Laufzeit für die Eingabelängen " ++ show args
	let falsch = do 
		x <- args
		let t = f x
		let ein = take  x $ repeat 'A'
		let ks = akzeptierend (t+1) m ein
		case ks of
			[] -> return ( ein, "erreicht innerhalb der ersten " ++ show t ++ " Schritte keinen Endzustand." )
			ks -> do k <- ks
				 guard $ nummer k /= t
				 return ( ein, "hält bereits im Schritt " ++ show (nummer k) ++ ", Laufzeit soll aber " ++ show t ++ " sein." )
	case  falsch of
	    [] -> do putStrLn $ "alle Laufzeiten sind korrekt"
		     right
	    wms -> do putStrLn $ unlines $ "diese Eingaben/Laufzeiten sind falsch:"
					 : map show wms

		      vorrechnens m $ take 3 $ map fst falsch
		      wrong

