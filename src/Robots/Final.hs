module Robots.Final where

--  $Id$

import Robots.Config
import Robots.Data
import Robots.Nice
import Autolib.Reporter.Type
import Autolib.ToDoc

import Data.List ( partition )
import Control.Monad ( guard )
import Data.Maybe

final :: Config -> Reporter ()
final k = do
    inform $ nice k 
    let robs = do r <- robots k
		  guard $ isJust $ ziel r
		  return ( ziel r == Just ( position r ) , r )
    let ( yeah, noh ) = partition fst robs
    inform $ vcat [ text "Sind alle Roboter an ihren Zielpunkten?"
			 , text "Diese ja: " <+> toDoc ( map snd yeah )
			 , text "Diese nicht: " <+> toDoc ( map snd noh )
			 ]
    assert ( null noh ) $ text "Geschafft?"
