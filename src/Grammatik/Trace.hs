module Grammatik.Trace where

import Grammatik.Type
import Grammatik.Ableitung
import Schichten

import Reporter
import Set
import ToDoc

type Track = [ String ]
type Tracks = [[ String ]]

nachfolger :: Int -> Grammatik -> String -> Set String
nachfolger b g u = mkSet $ do
    vss <- take b $ schichten ( schritt Nothing g ) $ cons u nil
    v <- setToList vss
    return $ car v

jump :: Int -> Grammatik -> (String, String) -> Reporter ()
jump b g (u, v) = do 
    let f = v `elementOf` nachfolger b g u
    inform $ fsep [ text "Gibt es eine kurze Ableitung"
		  , text "von" , toDoc u, text "nach", toDoc v, text "?"
		  , toDoc f
		  ]
    when ( not f ) $ reject empty

jumps :: Int -> Grammatik -> Track -> Reporter ()
jumps b g us = do
    inform $ text "Ich verifiziere die Kette" <+> toDoc us
    mapM_ (nested 4 . jump b g) $ zip us $ tail us
    newline

starts :: (Grammatik, Set String) -> Tracks -> Reporter ()
starts (g, vs) ts = do
    inform $ text "Beginnen alle Ableitungen mit dem Startsymbol?"
    let wrong = do
	      t <- ts
	      guard $ not $ take 1 t == [[startsymbol g]]
	      return t
    when (not $ null wrong) 
        $ reject $ text "diese nicht:" <+> toDoc wrong
    inform $ text "OK"
    newline

complete :: (Grammatik, Set String) -> Tracks -> Reporter ()
complete (g, vs) ts = do
    inform $ text "Haben Sie für jedes Zielwort eine Ableitung aufgeschrieben?"
    let terms = mkSet $ do t <- ts ; guard $ not $ null t ; return $ last t
    let wrong = vs `minusSet` terms
    when (not $ isEmptySet wrong)
	 $ reject $ text "nein, für diese nicht:" <+> toDoc wrong
    inform $ text "OK"
    newline

trace :: Int -> ( Grammatik, Set String ) -> Tracks -> Reporter Int
trace b ( g, vs ) ts = do
    inform $ text "Sie sollen nachweisen,"
    inform $ text "daß diese Wörter aus dem Startsymbol ableitbar sind:"
    inform $ nest 4 $ toDoc vs
    newline

    inform $ text "In den Ableitungen dürfen Sie jeweils maximal"
	   <+> toDoc b
	   <+> text "Einzelschritte zusammenfassen."
    newline

    inform $ text "Sie haben diese (verkürzten) Ableitungen eingesandt:"
    inform $ nest 4 $ toDoc ts
    newline

    starts (g, vs) ts
    inform $ text "Sind alle Ableitungsketten korrekt?"
    mapM_ ( nested 4 . jumps b g ) ts
    complete (g, vs) ts

    return 0

    
        
    
    

