module Turing_Akzeptieren 

where

-- $Log$
-- Revision 1.1  2003-04-14 05:47:20  joe
-- drift/todoc/reader
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2002/04/08 11:32:25  autotool
-- turing updates
--

import Turing
import Turing_Konfiguration
import Turing_Nachfolger
import Turing_Vorrechnen

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

