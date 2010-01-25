module Scorer.Compute where

--   $Id$

import Scorer.Aufgabe
import Scorer.Einsendung
import Scorer.Config
import Scorer.Emit

import Control.Types hiding ( size )
import Control.Aufgabe.Typ

import qualified Control.Vorlesung as V
import qualified Control.Schule as U

import Autolib.ToDoc
import Autolib.Output ( Output )
import qualified Autolib.Output as O

import Autolib.Util.Sort
import Autolib.FiniteMap
import Control.Monad ( guard )
import System ( getArgs )


-- | in fm steht abbildung von aufgabe(name) auf inhalt (z. b. direction)
compute :: U.Schule -> ( V.Vorlesung, ScoreDefFM ) -> IO ( Maybe Output )
compute u ( vor, aufs ) = do

    -- wir lesen die logfiles für jede vorlesung komplett neu ein,
    -- damit wir die entries, die wir nicht brauchen, 
    -- gleich wieder weghauen können

    args <- getArgs

    let (decorate,fileargs) = if null args then (False,[])
			      else ( case head args of
                                        "DECORATE" -> (True,tail args)
                                        "--cleartext" -> (False,tail args)
			                _ -> (True,args)
				   )

    contents <- mapM readFile fileargs
    let einsendungen = 
            filter Scorer.Einsendung.okay $ slurp $ concat contents

    let total = foldl ( update aufs ) emptyFM einsendungen
    -- pforsicht: hier sind auch die admins (< 1024) drin
    -- damit wir "best known" anzeigen können
    -- vor der bepunktung müssen die aber raus

    emit decorate u vor total

update :: ScoreDefFM -> DataFM -> Einsendung -> DataFM
update aufs mappe e = 
    case lookupFM aufs (auf e) of
        Nothing -> mappe -- aufgabe unbekannt
	Just a -> case highscore a of
	    Keine -> mappe -- keine wertung
	    dir  -> addToFM_C ( collide dir ) mappe (auf e) [ e ]

collide :: HiLo
	-> [ Einsendung ] -> [ Einsendung ] 
	-> [ Einsendung ]
collide dir schon neus = 
    let fun = case dir of
            Low -> id ; High -> negate 
    in    take ( scoreItems + 5 ) -- platz lassen für admins (s. o.)
	$ nubBy matrikel -- nur eine lösung je student
        $ mergeBy ( \ e -> ( fun ( size e ) -- erst nach größe
			    , date e )        -- dann nach zeit
		   ) neus schon

mergeBy fun xs ys = sortBy fun (xs ++ ys) -- FIXME


