module Robots2.Final where

--  $Id$

import Robots2.Config
import Robots2.Data
import Robots2.Nice
import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Set

import Data.List ( partition )
import Control.Monad ( guard )
import Data.Maybe

final :: Config -> Reporter ()
final k = do
    inform $ nice k 
    let ( yeah, noh ) = partition ( \ p -> p `elementOf` inhalt k) $ goals k
    inform $ vcat [ text "Sind alle Zielpunkte besetzt?"
			 , text "Diese ja: " <+> toDoc ( map snd yeah )
			 , text "Diese nicht: " <+> toDoc ( map snd noh )
			 ]
    assert ( null noh ) $ text "Geschafft?"
