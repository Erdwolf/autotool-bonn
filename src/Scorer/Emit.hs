module Scorer.Emit where

--   $Id$

import Scorer.Config
import Scorer.Einsendung
import Scorer.Aufgabe
import Scorer.Util

import Control.Types
import Control.Vorlesung.DB
import Control.Aufgabe.DB
import qualified Control.Vorlesung.Typ as V
import qualified Control.Aufgabe.Typ as A

import Autolib.FiniteMap
import Autolib.Set
import Autolib.Util.Sort

import Control.Monad ( guard )
import System.IO ( hFlush, stdout )


-- | druckt Auswertung f�r alle Aufgaben einer Vorlesung
emit :: VNr -> DataFM -> IO ()
emit vnr fm0 = do
    vs <- Control.Vorlesung.DB.get_this vnr
    let name = case vs of
          [v] -> toString $ V.name v 
          _   -> show vnr

    mnrs <- Control.Vorlesung.DB.teilnehmer vnr
    let smnrs = mkSet $ mnrs
    let fm = mapFM ( \ key val -> do
		  e <- val
		  guard $ matrikel e `elementOf` smnrs
		  return e
	      ) fm0

    putStrLn $ unlines
	     [ "", ""
	     ,  unwords [ "Auswertung f�r Lehrveranstaltung", name, ":" ] 
	     ]

    mapM_ single $ fmToList fm
    totalize fm
    inform


inform :: IO ()
inform = do
    putStrLn $ unlines 
	     [ unwords
	       [ "Dabei gibt es pro Score" , show scorePoints, "Punkte"
	       , "f�r die Pl�tze [1 ..", show scoreItems, "]" 
	       ]
	     , ""
	     ]

realize :: [ Einsendung ] -> [ Einsendung ]
realize es = take scoreItems -- genau 10 st�ck
	   $ filter ( (> 1023) . read . toString . matrikel) -- keine admins
	   $ es
    
-- | druckt Auswertung einer Aufgabe
single :: ( ANr, [ Einsendung ] ) -> IO ()
single arg @( anr, es ) = do
    aufs <- Control.Aufgabe.DB.get_this anr
    let name = case aufs of
          [a] -> toString $ A.name a 
          _   -> show anr
    let header = unwords 
	       [ "Aufgabe" , name
	       , unwords $ if null es then [] else
	         [ "( beste bekannte L�sung", show (size $ head es), ")" ]
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
	 ->  [ ( MNr, Int ) ] -- ^ ( Matrikel, Punkt )
collect fm = take scoreItems
	   $ sortBy ( \ (m, p) -> negate p ) -- gr��ten zuerst
	   $ fmToList
	   $ addListToFM_C (+) emptyFM
	   $ do  ( auf, es ) <- fmToList fm
		 ( e, p ) <- zip ( realize es ) scorePoints 
		 return ( matrikel e, p )


