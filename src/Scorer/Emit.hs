module Scorer.Emit where

--   $Id$

import Scorer.Config
import Scorer.Einsendung
import Scorer.Aufgabe
import Scorer.Util
import Data.FiniteMap
import Util.Sort

emit :: String -> DataFM -> IO ()
-- druckt Auswertung für alle Aufgaben einer vorlesung
emit vl fm = do
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
    
single :: ( String, [ Einsendung ] ) -> IO ()
-- druckt Auswertung einer Aufgabe
single ( auf, es ) = do
    let best = head es
	header = unwords [ "Aufgabe" , auf
			 , "( beste bekannte Lösung", show (size best), ")"  
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

collect :: DataFM 
	 ->  [ (Int, Int) ] -- ( Matrikel, Punkt )
-- gesamtliste der highscore
collect fm = take scoreItems
	   $  sortBy ( \ (m, p) -> negate p ) -- größten zuerst
	    $ fmToList
	    $ addListToFM_C (+) emptyFM
	    $ do ( auf, es ) <- fmToList fm
		 ( e, p ) <- zip ( realize es ) scorePoints 
		 return ( matrikel e, p )


