module Scorer.Aufgabe where

--   $Id$

import Scorer.Einsendung

import Data.FiniteMap
import ToDoc -- show FM
import SQLqueries 

-- (aufgabe -> aufgaben-record)
type ScoreDefFM = FiniteMap String Aufgabe

-- highscoretabelle (aufgabe -> beste sendungen)
type DataFM = FiniteMap String [ Einsendung ]


split_vorlesungen :: [ Aufgabe ] -> FiniteMap String ScoreDefFM
split_vorlesungen scts = 
    addListToFM_C ( plusFM_C (error "ScorerDB.plus")) emptyFM $ do
        sct <- scts
	return ( vorlesung sct , listToFM [ ( aufgabe sct, sct ) ] )

get :: IO ( FiniteMap String ScoreDefFM )
get = do
    -- die informationen zu den aufgaben holen
    sdDB <- getHighscoreAufgabeTypesDB
    let vls = split_vorlesungen sdDB
    return vls



