module Scorer.Emit where

--   $Id$

import Scorer.Config
import Scorer.Einsendung
import Scorer.Aufgabe
import Scorer.Util
import Data.FiniteMap
import Data.Set
import Util.Sort
import Control.Monad ( guard )
import System.IO ( hFlush, stdout )

import qualified SQLqueries

-- | druckt Auswertung für alle Aufgaben einer Vorlesung
emit :: String -> DataFM -> IO ()
emit vl fm0 = do

    mnrs <- SQLqueries.teilnehmer vl
    let smnrs = mkSet $ map read mnrs
    let fm = mapFM ( \ key val -> do
		  e <- val
		  guard $ matrikel e `elementOf` smnrs
		  return e
	      ) fm0

    putStrLn $ unlines
	     [ "", ""
	     ,  unwords [ "Auswertung für Lehrveranstaltung", vl, ":" ] 
	     ]

    mapM_ single $ fmToList fm
    totalize fm
    inform


inform :: IO ()
inform = do
    putStrLn $ unlines 
	     [ unwords
	       [ "Dabei gibt es pro Score" , show scorePoints, "Punkte"
	       , "für die Plätze [1 ..", show scoreItems, "]" 
	       ]
	     , ""
	     ]

realize :: [ Einsendung ] -> [ Einsendung ]
realize es = take scoreItems -- genau 10 stück
	   $ filter ( (> 1023) . matrikel) -- keine admins
	   $ es
    
-- | druckt Auswertung einer Aufgabe
single :: ( String, [ Einsendung ] ) -> IO ()
single arg @( auf, es ) = do
    let header = unwords 
	       [ "Aufgabe" , auf
	       , unwords $ if null es then [] else
	         [ "( beste bekannte Lösung", show (size $ head es), ")" ]
	       ]
	strich = replicate (length header) '-'

    putStrLn $ unlines 
	     $ [ header
	       , strich
	       ] ++ map show ( realize es )


totalize :: DataFM -> IO ()
totalize fm = do
    let mps = collect fm
    putStrLn $ unlines
	     $ [ "Top Ten"
	       , "-----------------------------------"
	       ] ++ do ( m, p ) <- mps
		       return $ unwords [ stretch 10 $ show p
					, ":"
					, stretch 10 $ show m 
					]

-- | gesamtliste der highscore
collect :: DataFM 
	 ->  [ (Int, Int) ] -- ^ ( Matrikel, Punkt )
collect fm = take scoreItems
	   $  sortBy ( \ (m, p) -> negate p ) -- größten zuerst
	   $ fmToList
	   $ addListToFM_C (+) emptyFM
	   $ do  ( auf, es ) <- fmToList fm
		 ( e, p ) <- zip ( realize es ) scorePoints 
		 return ( matrikel e, p )


