module Grammatik.Check 

-- $Id$

-- simpler test: einige wörter ableiten
-- und prüfen, ob sie zur sprache gehören

where

import Grammatik.Type
import Grammatik.Zeige

import Language.Type

import Reporter
import Size
import ToDoc
import List (partition)

check :: Language
      -> Int -- höchstens so lange satzformen
      -> Int -- höchstens soviele schichten
      -> Int -- höchstens soviele ableitungen anzeigen
      -> Int -- höchstens soviele terminalwörter zurückgeben
      -> Grammatik 
      -> Reporter Int
check lang l d a n g = do

     ws <- zeige l d a n g

     inform $ text
	    $ "Gehören diese Wörter zur Sprache " ++ abbreviation lang ++ " ?"
     let ( yeah, noh ) = partition ( contains lang ) ws
     when ( not $ null noh ) $ reject $ vcat
	  [ text "Nein, diese nicht:"
	  , nest 4 $ toDoc $ take 10 $ noh
	  ]
     inform $ text "OK."
     newline
     return $ size g
