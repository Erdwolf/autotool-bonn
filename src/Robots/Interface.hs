module Robots.Interface where

--  $Id$

import Robots.Data
import Robots.Config
import Robots.Move
import Robots.Nice
import Robots.Examples

import Autolib.Reporter
import Autolib.ToDoc

import Challenger.Partial
import Inter.Types

import Data.Maybe ( isJust )
import Data.List ( partition )

instance Partial Robots 
		 Config
		 [ Zug ]
	 where

    describe Robots k = vcat
        [ text "Lunar Lockout (c) Binary Arts:"
        , text "Geben Sie eine Zugfolge an,"
	, text "die den (die) Roboter (Groﬂbuchstaben)"
	, text "ins Ziel (entsprechende Kleinbuchstaben) bringen:"
	, nest 4 $ nice k
	]

    initial Robots k = 
        let a : b : _ = robots k
	in  [ ( name a, N ), ( name b, O ) ]

    partial Robots k zs = do
        executes k zs
	return ()

    total Robots k zs = do
        k' <- silent $ executes k zs
	final k'

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

make :: Make
make = direct Robots Robots.Examples.fourty


