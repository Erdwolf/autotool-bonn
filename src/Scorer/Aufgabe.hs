module Scorer.Aufgabe where

--   $Id$

import Scorer.Einsendung


import Control.Types
import Control.Aufgabe.Typ
import Control.Aufgabe.DB

import Autolib.FiniteMap

-- (aufgabe -> aufgaben-record)
type ScoreDefFM = FiniteMap ANr Aufgabe

-- highscoretabelle (aufgabe -> beste sendungen)
type DataFM = FiniteMap ANr [ Einsendung ]

split_vorlesungen :: [ Aufgabe ] -> FiniteMap VNr ScoreDefFM
split_vorlesungen aufs = 
    addListToFM_C ( plusFM_C (error "ScorerDB.plus")) emptyFM $ do
        auf <- aufs
	return ( vnr auf , listToFM [ ( anr auf, auf ) ] )

get :: IO ( FiniteMap VNr ScoreDefFM )
get = do
    -- die informationen zu den aufgaben holen
    aufs <- Control.Aufgabe.DB.get Nothing
    let vls = split_vorlesungen aufs
    return vls



