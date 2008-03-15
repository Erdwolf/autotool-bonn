module Grammatik.Trace where

import Grammatik.Type
import Grammatik.Ableitung
import qualified Grammatik.Ableitung.Config as A
import Grammatik.Trace.Config

import Autolib.Schichten

import Autolib.Reporter
import Autolib.Set
import Autolib.ToDoc

type Track = [ String ]
type Tracks = [[ String ]]

nachfolger :: Config -> String -> Set String
nachfolger conf u = mkSet $ do
    vss <- take ( A.max_depth $ search conf ) 
           $ schichten ( schritt ( search conf ) ( grammatik conf ) ) $ cons u nil
    v <- setToList vss
    return $ car v

jump :: Config -> (String, String) -> Reporter ()
jump conf (u, v) = do 
    let f = v `elementOf` nachfolger conf u
    inform $ fsep [ text "Gibt es eine kurze Ableitung"
		  , text "von" , toDoc u, text "nach", toDoc v, text "?"
		  , toDoc f
		  ]
    when ( not f ) $ reject empty

jumps :: Config -> Track -> Reporter ()
jumps conf us = do
    inform $ text "Ich verifiziere die Kette" <+> toDoc us
    mapM_ (nested 4 . jump conf ) $ zip us $ tail us
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

trace :: Config ->  Set String -> Tracks -> Reporter Int
trace conf vs ts = do
    let g = grammatik conf
        b = A.max_depth $ search conf
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
    mapM_ ( nested 4 . jumps conf ) ts
    complete (g, vs) ts

    return 0

    
        
    
    

