module Shift.Verify where

import Shift.Type
import Shift.Computer
import Util.Faktor

import Reporter
import ToDoc


verify :: Int -> Shift -> Reporter Int
verify limit sh = do
    let ps = pins sh
    let q = vorperiode sh
    let p = periode sh

    when ( q > limit || p > limit ) 
	 $ reject $ fsep [ text "Eine (Vor-)Periode über", toDoc limit
			 , text "kann ich nicht verifizieren,"
			 , text "weil das zuviel Rechenkraft verbraucht."
			 ]

    -- das rechnen wir zweimal aus, damit es jedesmal (hoffentlich)
    -- in konstantem space läuft
    let sq  = zustands_folge ps !! q
    let sqp = zustands_folge ps !! (q + p)

    inform $ fsep [ text "Stimmen die Zustände zu den Zeiten"
		  , toDoc q, text "und", toDoc (p+q), text "überein?"
		  ]
    when ( sq /= sqp ) 
	 $ reject $ text "Nein."
    inform $ text "Ja."
    newline

    inform $ vcat [ text "Gibt es eine kleinere Periode?"
		  , text "Prüfe für alle echten Primteiler von" <+> toDoc p
		  ]
    newline
    sequence $ do
        t <- map fromIntegral $ primfaktoren $ fromIntegral p
	guard $ t < p
	return $ do
	    let pt = p `div` t
	    let sqpt = zustands_folge ps !! (q + pt)
	    let flag = ( sq /= sqpt ) 
	    inform $ fsep [ text "Teiler", toDoc t, comma
			  , text "Teilperiode", toDoc pt, comma
			  , text "Z" <> parens (toDoc q)
			  , text "/=", text "Z" <> parens (toDoc (q+pt))
			  , text "...", toDoc flag
			  ]
	    when ( not flag )
		 $ reject $ text "also war" <+> toDoc p
			  <+> text "nicht die kürzeste Periode."
	
    return p

    
