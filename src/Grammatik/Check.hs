module Grammatik.Check 

-- -- $Id$

-- simpler test: einige w�rter ableiten
-- und pr�fen, ob sie zur sprache geh�ren

where

import Grammatik.Type
import Grammatik.Zeige

import Grammatik.Ableitung ( Config (..) )

import Language.Type

import Autolib.Reporter
import Autolib.Size
import Autolib.ToDoc
import Data.List (partition)

check :: Language
      -> [ String ]
      -> Reporter ()
check lang ws = do
     inform $ text
	    $ "Geh�ren diese W�rter zur Sprache " ++ abbreviation lang ++ " ?"
     let ( yeah, noh ) = partition ( contains lang ) ws
     when ( not $ null noh ) $ reject $ vcat
	  [ text "Nein, diese nicht:"
	  , nest 4 $ toDoc $ take 10 $ noh
	  ]
     inform $ text "OK."

