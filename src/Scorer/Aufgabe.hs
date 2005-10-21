module Scorer.Aufgabe where

--   $Id$

import Scorer.Einsendung


import Control.Types
import qualified Control.Aufgabe as A

import Autolib.FiniteMap

-- (aufgabe -> aufgaben-record)
type ScoreDefFM = FiniteMap ANr A.Aufgabe

-- highscoretabelle (aufgabe -> beste sendungen)
type DataFM = FiniteMap ANr [ Einsendung ]

split_vorlesungen :: [ A.Aufgabe ] -> FiniteMap VNr ScoreDefFM
split_vorlesungen aufs = 
    addListToFM_C ( plusFM_C (error "ScorerDB.plus")) emptyFM $ do
        auf <- aufs
	return ( A.vnr auf , listToFM [ ( A.anr auf, auf ) ] )

get :: IO ( FiniteMap VNr ScoreDefFM )
get = do
    -- die informationen zu den aufgaben holen
    aufs <- A.get Nothing
    let vls = split_vorlesungen aufs
    return vls



