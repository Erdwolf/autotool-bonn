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

zeige :: Int -- höchstens so lange satzformen
      -> Int -- höchstens soviele schichten
      -> Int -- höchstens soviele ableitungen anzeigen
      -> Int -- höchstens soviele terminalwörter zurückgeben
      -> Grammatik 
      -> Reporter [ String ]
zeige l d a n g = do

    let abss = take d -- schichten
	     $ ableitungen ( Just l ) g

    inform $ text "Ich zeige Ihnen für die Grammatik"
    inform $ nest 4 $ toDoc g
    inform $ text "einige Ableitungen:"
    inform $ nest 4 $ vcat $ take a $ do 
	       abs <- drop ( d `div` 2 ) abss
	       ab  <- setToList abs
	       return $ toDoc ab
    newline
	   
    let terms = take n
	      $ do abs <- abss
		   ab <- setToList abs
		   let w = car ab
		   guard $ all (`elementOf` terminale g) w
		   return w
    inform $ text "Einige ableitbare Wörter aus Terminalzeichen sind:"
    inform $ nest 4 $ toDoc $ take 20 terms
    newline

    return terms


