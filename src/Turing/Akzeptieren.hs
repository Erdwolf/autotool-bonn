module Turing.Akzeptieren 

where

-- -- $Id$

import Turing
import Turing.Konfiguration
import Turing.Nachfolger
import Turing.Vorrechnen

import Monad (guard)
import Reporter
import ToDoc

akzeptierend
    :: (TUM y z)
    => Int -> Turing y z -> [y] -> [ Konfiguration y z ]
akzeptierend cut m xs = do
    k <- take cut $ nachfolger m (start_konfiguration m xs)
    guard $ zustand k `elementOf` endzustandsmenge m
    return k


positiv_liste :: (TUM y z)
	      => Int -> Turing y z -> [[y]]
	      -> Reporter ()
positiv_liste cut m  xss = do
     let fehler = take 3 $ do 
            xs <- xss
            let ks = akzeptierend cut m xs
            guard $ null ks -- d. h. fälschlicherweise nicht akzeptiert
            return xs
     case fehler of
          []  -> inform $ text "alle Wörter aus der Positiv-Liste wurden akzeptiert"
          xss -> do
	     inform $ text "diese Wörter der Positiv-Liste wurden nicht akzeptiert:"
	     inform $ toDoc xss
	     vorrechnens m xss


negativ_liste :: (TUM y z)
	      => Int -> Turing y z -> [[y]] 
              -> Reporter ()
negativ_liste cut m xss = do
    let fehler = take 3 $ do 
         xs <- xss
         let ks = akzeptierend cut m xs
         guard $ not $ null ks -- d. h. fälschlicherweise akzeptiert
         return xs
    case fehler of 
	 []  -> inform $ text $ "jedes Wort aus der Negativ-Liste wurden nicht akzeptiert"
         xss -> do 
	     inform $ text "diese Wörter der Negativ-Liste wurden doch akzeptiert:"
	     inform $ toDoc xss
	     vorrechnens m xss


