module Robots.Interface where

-- $Id$

import Robots.Data
import Robots.Konfig
import Robots.Move
import Robots.Nice


import Reporter
import ToDoc

import Challenger.Partial
import Inter.Types

import Data.Maybe ( isJust )
import Data.List ( partition )

instance Partial Robots 
		 Konfig
		 [ Zug ]
	 where

    describe Robots k = vcat
        [ text "Lunar Lockout." 
	, text "Finden Sie eine Zugfolge für diese Konfiguration:"
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

------------------------------------------------------------------------------

make :: String -- aufgabe (major)
     -> String -- aufgabe (minor)
     -> Konfig 
     -> Var Robots Konfig [ Zug ]
make auf ver conf = 
    Var { problem = Robots
        , aufgabe = auf
        , version = ver
        , key = \ matrikel -> do 
	      return matrikel
        , gen = \ key -> do
	      return $ return conf
        }



