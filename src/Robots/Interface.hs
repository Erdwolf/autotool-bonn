module Robots.Interface where

-- $Id$

import Robots.Data
import Robots.Konfig
import Robots.Move
import Robots.Examples

import ToDoc

import Challenger.Partial

instance Partial Robots 
		 Konfig
		 [ Zug ]
	 where

    describe Robots k = vcat
        [ "Lunar Lockout: Finden Sie eine Zugfolge für diese Konfiguration:"
	, nest 4 $ nice k
	]

    initial Robots k = 
        let a : b : _ = robots k
	in  [ ( name a, N ), ( name b, O ) ]

    partial Robots k zs = do
        executes k zs

    total Robots k zs = do
        k' <- silent $ executes k zs
	final k'

final :: Konfig -> Reporter ()
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

