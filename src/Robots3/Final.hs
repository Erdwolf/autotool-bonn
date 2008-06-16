module Robots3.Final where

--  $Id$

import Robots3.Config
import Robots3.Data
import Robots3.Nice
import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Set

import Data.List ( partition )
import Control.Monad ( guard )
import Data.Maybe

final :: Config -> Reporter ()
final k = do
    inform $ nice k 
    let ps = mkSet $ positions k
    let ( yeah, noh ) = partition ( \ p -> p `elementOf` ps )  $ goals k
    inform $ vcat [ text "Sind alle Zielpunkte belegt?"
			 , text "Diese ja: " <+> toDoc yeah
			 , text "Diese nicht: " <+> toDoc noh
			 ]
    assert ( null noh ) $ text "Geschafft?"

is_final k = 
    let ps = mkSet $ positions k
        ( yeah, noh ) = partition ( \ p -> p `elementOf` ps )  $ goals k
    in  null noh

