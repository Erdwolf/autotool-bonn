module Grammatik.Zeige

-- $Id$

( zeige
)

where

import Grammatik.Type
import Grammatik.Ableitung

import Set

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
    inform $ nest 4 $ vcat $ do 
	       abs <- drop ( max_depth conf `div` 2 ) abss
	       ab  <- setToList abs
	       return $ toDoc ab
    newline
	   
    let terms = do abs <- abss
		   ab <- setToList abs
		   let w = car ab
		   guard $ all (`elementOf` terminale g) w
		   return w
    inform $ text "Einige ableitbare Wörter aus Terminalzeichen sind:"
    inform $ nest 4 $ toDoc $ take 20 terms
    newline

    return terms


