module Boolean.Instance where

--  $Id$

import Boolean.Op
import Boolean.Data
import qualified Boolean.Equiv

import Challenger.Partial
import qualified Reporter.Subset

import Data.Typeable

import ToDoc
import Reporter
import Sets

data Boolean = Boolean deriving ( Eq, Ord, Show, Read )

data BI =
     BI { tag :: String
        , formula :: Exp
	, operators :: Set Op
	}
     deriving ( Show, Typeable )

instance Partial Boolean BI Exp where

    describe p i = vcat
        [ text "Gesucht ist ein aussagenlogischer Ausdruck,"
	, text "der �quivalent ist zu:"
	, nest 4 $ toDoc $ formula i
	, text ""
	, parens $ vcat [ text "die Baumstruktur dieses Ausdrucks ist:"
			, nest 4 $ draw  $ formula i
			]
	, text "und nur diese Operatoren enth�lt:"
	, nest 4 $ toDoc $ operators i
	]

    initial p i = formula i

    partial p i b = do
        inform $ vcat
	       [ text "Die Baumstruktur Ihrer Einsendung ist:"
	       , nest 4 $ draw b
	       , text ""
	       ]
        Reporter.Subset.check
	    ( parens $ text "benutzte Operatoren" , syms b )
	    ( parens $ text "erlaubte Operatoren" , operators i )
	Reporter.Subset.check
	    ( parens $ text "benutzte Variablen" , vars b )
	    ( parens $ text "Variablen der Aufgabenstellung",vars $ formula i )

    total p i b = do
        Boolean.Equiv.check ( formula i ) b




