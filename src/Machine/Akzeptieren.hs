module Machine.Akzeptieren 

where

-- $Id$

import Machine.Class
import Machine.Vorrechnen

import Monad (guard)
import Reporter
import ToDoc

akzeptierend
    :: Machine m dat conf
    => Int -> m -> dat -> [ conf ]
akzeptierend cut m xs = do
    k <- take cut $ nachfolger m $ input m xs
    guard $ accepting m k
    return k


positiv_liste :: ( Machine m dat conf, ToDoc [dat] )
	      => Int -> m -> [dat]
	      -> Reporter ()
positiv_liste cut m  xss = do
     let fehler = take 3 $ do 
            xs <- xss
            let ks = akzeptierend cut m xs
            guard $ null ks -- d. h. fälschlicherweise nicht akzeptiert
            return xs
     case fehler of
          []  -> inform $ text "alle Eingaben aus der Positiv-Liste wurden akzeptiert"
          xss -> do
	     inform $ text "diese Eingaben der Positiv-Liste wurden nicht akzeptiert:"
	     inform $ toDoc xss
	     vorrechnens m xss


negativ_liste :: ( Machine m dat conf, ToDoc [dat] )
	      => Int -> m -> [dat] 
              -> Reporter ()
negativ_liste cut m xss = do
    let fehler = take 3 $ do 
         xs <- xss
         let ks = akzeptierend cut m xs
         guard $ not $ null ks -- d. h. fälschlicherweise akzeptiert
         return xs
    case fehler of 
	 []  -> inform $ text $ "jedes Eingabe aus der Negativ-Liste wurden nicht akzeptiert"
         xss -> do 
	     inform $ text "diese Eingaben der Negativ-Liste wurden doch akzeptiert:"
	     inform $ toDoc xss
	     vorrechnens m xss



