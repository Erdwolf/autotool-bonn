module Scorer.Einsendung 

( Einsendung (..)
, slurp -- datei-inhalt verarbeiten
)

where

--   $Id$

{- so sehen die dinger aus:

Fri Nov 28 18:33:49 CET 2003 ( 2425 ) cgi-318 ( 318 ) Ein-Gleich : OK # Size: 7 
-}

import Scorer.Util

import Data.FiniteMap
import Control.Monad ( guard )

-- das ist die information zu jeweils einer studentischen einsendung
data Einsendung = Einsendung
          { size     :: Int
	  , date     :: [Int]
	  , time     :: String -- original time entry
	  , matrikel :: Int
	  , auf	     :: String
	  , pid	     :: String
	  }	deriving (Eq,Ord)

instance Show Einsendung where
    show i = unwords 
        [ spaci 4 $ abs $ size i
		, spaci 8 $ matrikel i
		,    (nulli 2 $ (date i) !! 2) ++ "."
		  ++ (nulli 2 $ (date i) !! 1) ++ "."
		  ++ (nulli 4 $ (date i) !! 0) 
		,    (nulli 2 $ (date i) !! 3) ++ ":"
		  ++ (nulli 2 $ (date i) !! 4) ++ ":"
		  ++ (nulli 2 $ (date i) !! 5)
		]

spaci :: Show a => Int -> a -> String
spaci n = stretch n . show

nulli :: Show a => Int -> a -> String
nulli n = stretchWith '0' n . show


slurp :: String -> [ Einsendung ]
-- alle lesbaren Zeilen
slurp cs = do
    z <- lines cs
    ( e, _ ) <- readsPrec 0 z
    return e

instance Read Einsendung where 
    readsPrec p cs = do
        let ( line, rest ) = span (/= '\n') cs

	let
            field n	 = head . drop (n-1)
            mySub x | x == ':'  = ' '
            		  | otherwise = x
            line'   = dropWhile (/=")") wl
            date'   = takeWhile (/="(") wl
            wl      = words line
            aufg    = field 6 line'
	    ok	    = field 8 line'

	guard $ ok == "OK"

	let e = Einsendung
	      {	time = unwords $ take 6 wl
              , date = [ read     $ field 6 date'            -- Jahr
                       , monthNum $ field 2 date'            -- Monat
                       , read     $ field 3 date'            -- Tag
                       ]
                       ++                                    -- St:Mi:Se
                       [ read x | x <- words $ map mySub (field 4 date') ]
	      , matrikel = read $ field  4 line'
	      , size     = read $ field 11 line'
	      , auf	 = aufg
	      , pid      = field 8 wl			-- process id
	      }
	return ( e, rest )

-------------------------------------------------------------------------------
-- komisch, aber ich habs nirgendwo gefunden

monthFM :: FiniteMap String Int 
monthFM    = listToFM [ ("Jan", 1),("Feb", 2),("Mar", 3),("Apr", 4)
		      , ("May", 5),("Jun", 6),("Jul", 7),("Aug", 8)
		      , ("Sep", 9),("Oct",10),("Nov",11),("Dec",12)
                      ]

-- Umwandlung Monat-K�rzel -> Zahl, bei Fehler kommt 13 zur�ck
monthNum m = lookupWithDefaultFM monthFM 13 m
