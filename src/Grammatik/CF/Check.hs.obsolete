module Grammatik.CF_Check where

-- $Id$

import Language.Type

import Grammatik.Trace

import Grammatik.Type
import Grammatik.Check

import Grammatik.Epsfrei
import Grammatik.Kettenfrei

import Grammatik.Hierarchie ( typ2 )

import qualified Grammatik.Chomsky as C


-- eins der beiden imports auswählen:

-- für jedes wort einzeln das CYK-array ausrechnen
-- (besser für hugs??)
-- import CYK

-- gemeinsamen speicher für alle teilwörter
-- (müßte eigentlich hashtabelle benutzen, FiniteMaps sind zu langsam?)
import Grammatik.DPL_CYK

-- end auswahl

import Size
import Set

import List (partition, nub, sortBy)
import FilterBound

import ToDoc
import Random

import Util.Seed
import Util.Wort
import Util.Zufall


import Reporter
import qualified Reporter.Result

cf_check :: Language 
	 -> ( Grammatik -> Reporter () ) -- form-test (z. b. greibach)
	 -> Int -- anzahl der samples
	 -> Int -- minimale länge der samples
	 -> [Int] -- längen der demo-wörter (für trace)
	 -> Int -- schrittweite für trace
	 -> String -- matrikelnummer
	 -> ( Grammatik, Tracks ) -- einsendung
	 -> IO ( Maybe Int )
cf_check l typ w n ds t mat ( g, ts ) = do

    seed $ read mat

    -- die kleinen sollten ja auch schnell zu testen sein
    let klein = take 100 $ do n <- [0 .. ] ; alle ( setToList $ alphabet l ) n

    here   <- samples      l w n
    there  <- anti_samples l w n
    let (yeah, noh) = partition (contains l) $ nub $ klein ++ here ++ there

    let handle d = do
	    ws <- samples l d d
	    eins ws
    demos  <- mapM handle ds

    Reporter.Result.wrapper 
		$ cf_yeah_noh l yeah noh typ (mkSet demos) t ( g, ts )


cf_yeah_noh :: Language
	    -> [ String ] -- soll produzieren
	    -> [ String ] -- soll nicht produzieren
	    -> ( Grammatik -> Reporter () ) -- typ
	    -> Set String  -- soll demonstrieren
	    -> Int -- schrittweite für trace
	    -> ( Grammatik, Tracks )  -- eingabe
	    -> Reporter Int
cf_yeah_noh lang yeah0 noh0 typ demos t gts @ ( g, ts ) = do
    let doc = text $ abbreviation lang
    inform $ text "Gesucht sind Grammatik und Beispiel-Ableitungen für" <+> doc
    newline
    inform $ text "Ihre Grammatik ist"
    inform $ toDoc g
    newline

    typ2 g
    typ g

    check lang 12 -- max wortlänge 
	        7 -- max schichttiefe
		5 -- max abl anz
	      100 -- max wörter zurück
	        g

    let arrange = sortBy (\ x y -> compare (length x, x) (length y, y))
    let [ yeah, noh ] = map arrange [ yeah0, noh0 ]

    trace t ( g, demos ) ts 

    -- wenn wir hier ankommen, sind die ableitungen OK

    let ch = C.chomsky $ kettenfrei $ epsfrei $ g
    let snip = take 5 -- ??

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter der Zielsprache"
	   , toDoc yeah
	   , text "von Ihrer Grammatik erzeugt werden."
	   ]

    let achyeah = accepteds ch yeah
    let yeah_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> not f ) 
		    $ achyeah

    when ( not $ null yeah_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter NICHT:"
	 , toDoc yeah_falsch
	 ]
    inform $ text "OK."

    inform $ vcat
	   [ text "Ich prüfe, ob diese Wörter aus dem Komplement der Zielsprache"
	   , toDoc noh
	   , text "tatsächlich NICHT von Ihrer Grammatik erzeugt werden."
	   ]

    let achnoh =  accepteds ch noh
    let noh_falsch = snip 
		    $ map ( \ (w, f) -> w )
		    $ filterBound 5 ( \ (w, f) -> f ) 
		    $ achnoh

    when ( not $ null noh_falsch ) $ reject $ vcat
	 [ text "Ihre Grammatik erzeugt unter anderem diese Wörter DOCH:"
	 , toDoc noh_falsch
	 ]
    inform $ text "OK."

    -- wenn wir hier sind, ist alles OK
    return $ size g







