module Grammatik.Check 

-- $Id$

-- simpler test: einige wörter ableiten
-- und prüfen, ob sie zur sprache gehören

where

import Grammatik.Type
import Grammatik.Zeige

import Grammatik.Ableitung ( Config (..) )

import Language.Type

import Reporter
import Size
import ToDoc
import List (partition)

check :: Language
      -> Config
      -> Grammatik 
      -> Reporter Int
check lang conf g = do

     ws <- zeige conf g

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
