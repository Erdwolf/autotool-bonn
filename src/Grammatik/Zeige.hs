module Grammatik.Zeige

-- -- $Id$

( zeige
)

where

import Grammatik.Type
import Grammatik.Ableitung

import Grammatik.Reduziert

import Data.Set

import Util.Wort

import Reporter
import ToDoc

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x : xs) = x : if p x then [] else takeUntil p xs

zeige :: Config
      -> Grammatik 
      -> Reporter [ String ]
zeige conf g = do

    let abss = ableitungen conf g

    inform $ text "Ich zeige Ihnen einige Ableitungen:"
    inform $ nest 4 $ vcat $ take 10 $ do 
	       abs <- drop ( max_depth conf `div` 2 ) abss
	       ab  <- setToList abs
	       return $ toDoc ab
    newline
	
    let rg = reduktion g -- nur erreichbare und produktive, 
	                 -- damit das tool nicht sinnlos rumrechnet

    -- inform $ text "DEBUG: reduziert:" <+> toDoc rg

    if ( not $ startsymbol rg `elementOf` nichtterminale rg )
       then do
	    inform $ text "Es sind keine Terminalwörter ableitbar."
	    return []
       else do
            let rabss = ableitungen conf rg
		terms = do 
		   abs <- rabss
		   ab <- setToList abs
		   let w = car ab
		   guard $ all (`elementOf` terminale g) w
		   return w
	    inform $ text "Einige (in wenigen Schritten) ableitbare Terminalwörter sind:"
	    inform $ nest 4 $ toDoc $ take 20 terms
	    newline
	    return terms


