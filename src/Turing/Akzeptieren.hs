module Turing.Akzeptieren 

where

-- $Id$

import Turing
import Turing.Konfiguration
import Turing.Nachfolger
import Turing.Vorrechnen

import Monad (guard)

akzeptierend
    :: (TUM y z)
    => Int -> Turing y z -> [y] -> [ Konfiguration y z ]
akzeptierend cut m xs = do
    k <- take cut $ nachfolger m (start_konfiguration m xs)
    guard $ zustand k `elementOf` endzustandsmenge m
    return k


positiv_liste :: (TUM y z)
	      => Int -> Turing y z -> [[y]] -> Either String String
positiv_liste cut m  xss = case take 3 $
    do xs <- xss
       let ks = akzeptierend cut m xs
       guard $ null ks -- d. h. fälschlicherweise nicht akzeptiert
       return xs
  of []  -> Right $ "alle Wörter aus der Positiv-Liste wurden akzeptiert"
     xss -> Left  $ unlines 
		  $ [ "diese Wörter der Positiv-Liste wurden nicht akzeptiert:"
		    , unwords $ map show xss
		    ] 
		    ++ concat ( map (vorrechnen m) xss )

negativ_liste :: (TUM y z)
	      => Int -> Turing y z -> [[y]] -> Either String String
negativ_liste cut m xss = case take 3 $
    do xs <- xss
       let ks = akzeptierend cut m xs
       guard $ not $ null ks -- d. h. fälschlicherweise akzeptiert
       return xs
  of []  -> Right $ "jedes Wort aus der Negativ-Liste wurden nicht akzeptiert"
     xss -> Left  $ unlines 
		  $ [ "diese Wörter der Negativ-Liste wurden doch akzeptiert:"
		    , unwords $  map show xss
		    ]  
		    ++ concat ( map (vorrechnen m) xss )

