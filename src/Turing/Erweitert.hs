module Turing.Erweitert where

-- $Id$

import Turing.Type
import Reporter
import ToDoc

erweitert :: TUM y z => Turing y z -> Turing y z -> Reporter ()
-- ist a erweiterung von b?
erweitert a b = do
    inform $ text "ist Ihre Maschine eine Erweiterung der gegebenen?"
    assert ( eingabealphabet a == eingabealphabet b )
	   ( text "sind die Eingabealphabete gleich?" )
    assert ( leerzeichen a == leerzeichen b )
	   ( text "sind die Leerzeichen gleich?" )
    assert ( subseteq ( zustandsmenge b ) (zustandsmenge a ) )
	   ( text "ist die originale Zustandsmenge enthalten?" )
    assert ( subseteq ( arbeitsalphabet b ) (arbeitsalphabet a ) )
	   ( text "ist das originale Arbeitsalphabet enthalten?" )
    assert ( subseteq ( endzustandsmenge b ) (endzustandsmenge a ) )
	   ( text "ist die originale Endzustandsmenge enthalten?" )
    assert ( startzustand a == startzustand b )
	   ( text "sind die Startzustände gleich?" )

    let ucb = unCollect $ tafel b
	uca = unCollect $ tafel a
        miss = minusSet ( mkSet ucb ) ( mkSet uca )
    inform $ text "ist die originale Übergangsrelation enthalten?"
    when ( not $ isEmptySet miss ) $ reject
	 $ text "Nein, diese Tupel fehlen:" <+> toDoc miss
    inform $ text "Ja."


