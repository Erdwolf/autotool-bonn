module Scorer.Compute where

-- -- $Id$

import Scorer.Aufgabe
import Scorer.Einsendung
import Scorer.Config
import Scorer.Emit

import Util.Sort
import SQLqueries ( ATHighLow (..), Aufgabe (..) )
import Data.FiniteMap
import Control.Monad ( guard )
import System ( getArgs )

compute :: ( String, ScoreDefFM ) -> IO ()
-- in fm steht abbildung von aufgabe(name) auf inhalt (z. b. direction)
compute ( vl, aufs ) = do

    -- wir lesen die logfiles f�r jede vorlesung komplett neu ein,
    -- damit wir die entries, die wir nicht brauchen, 
    -- gleich wieder weghauen k�nnen

    args <- getArgs
    contents <- mapM readFile args
    let einsendungen = slurp $ concat contents

    let total = foldl ( update aufs ) emptyFM einsendungen
    -- pforsicht: hier sind auch die admins (< 1024) drin
    -- damit wir "best known" anzeigen k�nnen
    -- vor der bepunktung m�ssen die aber raus
    emit vl total

update :: ScoreDefFM -> DataFM -> Einsendung -> DataFM
update aufs mappe e = 
    case lookupFM aufs (auf e) of
        Nothing -> mappe -- aufgabe unbekannt
	Just a -> case direction a of
	    Keine -> mappe -- keine wertung
	    dir  -> addToFM_C ( collide dir ) mappe (auf e) [ e ]

collide :: ATHighLow 
	-> [ Einsendung ] -> [ Einsendung ] 
	-> [ Einsendung ]
collide dir schon [ neu ] = 
    let fun = case dir of
            Low -> id ; High -> negate
    in    take ( scoreItems + 5 ) -- platz lassen f�r admins (s. o.)
	$ nubBy matrikel -- nur eine l�sung je student
        $ insertBy ( \ e -> ( fun ( size e ) -- erst nach gr��e
			    , date e )        -- dann nach zeit
		   ) neu schon



