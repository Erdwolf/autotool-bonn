module Turing.Vorrechnen

where

-- $Id$

import Turing
import Turing.Konfiguration
import Turing.Nachfolger
import Schichten

vorrechnen :: TUM y z
	   => Turing y z -> [y] -> Strings
vorrechnen a xs = let cut = 10 in
    [ ""
    , "mit Eingabe " ++ show xs ++ " erreicht der Automat folgende Konfigurationen:"
    , "(ich zeige maximal die ersten " ++ show cut ++ " Schritte)"
    ] ++ ( indent 4 $ do
        (i, ks) <- zip [0..] 
		$ take cut
		$ map setToList 
		$ schichten (folgekonfigurationen a   )
		$ start_konfiguration a xs
	( "nach " ++ show i ++ " Schritten:" )
	       : ( indent 4 $ do k <- ks; text k )
    )
    
vorrechnens :: TUM y z
	   => Turing y z -> [[y]] -> IO ()
vorrechnens a = putStrLn . unlines . concat . map (vorrechnen a)

-----------------------------------------------------------------------

type Strings = [ String ]
class Text a where text :: a -> Strings

indent :: Int -> Strings  -> Strings
indent i lines = do
    let prefix = take i $ repeat ' '
    l <- lines
    return $ prefix ++ l

instance  TUM y z
	  => Text (Konfiguration y z) where text k = [ show k ]
instance  TUM y z
	  => Text (Turing y z) where text a = lines (show a)

