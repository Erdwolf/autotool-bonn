module Grammatik.Check 

-- $Id$

-- simpler test: einige w�rter ableiten
-- und pr�fen, ob sie zur sprache geh�ren

where

import Grammatik.Type
import Grammatik.Zeige

import Language.Type

import Reporter
import Size
import ToDoc
import List (partition)

check :: Language
      -> Int -- h�chstens so lange satzformen
      -> Int -- h�chstens soviele schichten
      -> Int -- h�chstens soviele ableitungen anzeigen
      -> Int -- h�chstens soviele terminalw�rter zur�ckgeben
      -> Grammatik 
      -> Reporter Int
check lang l d a n g = do

     ws <- zeige l d a n g

     inform $ text
	    $ "Geh�ren diese W�rter zur Sprache " ++ abbreviation lang ++ " ?"
     let ( yeah, noh ) = partition ( contains lang ) ws
     when ( not $ null noh ) $ reject $ vcat
	  [ text "Nein, diese nicht:"
	  , toDoc $ take 10 $ noh
	  ]
     inform $ text "OK."
     newline
     return $ size g
