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

zeige :: Int -- h�chstens so lange satzformen
      -> Int -- h�chstens soviele schichten
      -> Int -- h�chstens soviele ableitungen anzeigen
      -> Int -- h�chstens soviele terminalw�rter zur�ckgeben
      -> Grammatik 
      -> Reporter [ String ]
zeige l d a n g = do

    let abss = take d -- schichten
	     $ takeUntil ( \ s -> cardinality s > n )
	     $ ableitungen ( Just l ) g

    inform $ text "Ich zeige Ihnen einige Ableitungen:"
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
    inform $ text "Einige ableitbare W�rter aus Terminalzeichen sind:"
    inform $ nest 4 $ toDoc $ take 20 terms
    newline

    return terms


