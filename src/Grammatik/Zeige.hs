module Grammatik.Zeige

-- -- $Id$

( zeige
)

where

import Grammatik.Type
import Grammatik.Ableitung
import Grammatik.Ableitung.Config

import Grammatik.Reduziert

import Autolib.Set

import Autolib.Util.Wort

import Autolib.Reporter
import Autolib.ToDoc

zeige :: Config
      -> Grammatik 
      -> Reporter [ String ]
zeige conf g = do
    zeige_ableitungen conf g
    -- nur erreichbare und produktive, 
    -- damit das tool nicht sinnlos rumrechnet
    zeige_woerter conf $ reduktion g

zeige_ableitungen :: Config
      -> Grammatik 
      -> Reporter ()
zeige_ableitungen conf g = do
    let abss = ableitungen conf g
    inform $ vcat 
	   [ text "Hier sehen Sie einige Ableitungen:"
           , nest 4 $ vcat $ take 10 $ do 
	       abs <- drop ( max_depth conf `div` 2 ) abss
	       ab  <- setToList abs
	       return $ toDoc ab
	   ]

zeige_woerter :: Config
      -> Grammatik 
      -> Reporter [ String ]
zeige_woerter conf g = do
    if ( not $ startsymbol g `elementOf` nichtterminale g )
       then do
	    inform $ text "Es sind keine Terminalwörter ableitbar."
	    return []
       else do
            let rabss = ableitungen conf g
		terms = do 
		   abs <- rabss
		   ab <- setToList abs
		   let w = car ab
		   guard $ all (`elementOf` terminale g) w
		   return w
	    inform $ vcat
		   [ text "Einige (in wenigen Schritten) ableitbare Terminalwörter sind:"
		   , nest 4 $ toDoc $ take 20 terms
		   ]
	    return terms

{-
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x : xs) = x : if p x then [] else takeUntil p xs
-}


