-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Boolean.Instance where

--  $Id$

import Boolean.Op
import qualified Autolib.TES.Binu as B
import qualified Boolean.Equiv

import Challenger.Partial
import qualified Autolib.Reporter.Set

import Data.Typeable

import Autolib.ToDoc
import Autolib.Multilingual
import Autolib.Reader
import Autolib.Reporter
import Autolib.Set

import Inter.Types

data Boolean = Boolean deriving ( Eq, Ord, Typeable )

instance OrderScore Boolean where
    scoringOrder _ = Increasing

$(derives [makeReader, makeToDoc] [''Boolean])

data BI =
     BI { formula :: Exp Bool
	, operators :: Set ( Op Bool )
	}
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''BI])

data BIC =
     BIC { formula_size :: Int
	 , operators_in_instance :: B.Binu ( Op Bool )
	 , operators_in_solution :: Set ( Op Bool )
	 }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''BIC])

instance Partial Boolean BI ( Exp Bool ) where

    describe p i = vcat
        [ multitext [ (DE, "Gesucht ist ein aussagenlogischer Ausdruck, der äquivalent ist zu:")
		    , (UK, "Find an expression in propositional logic that is equivalent to:")
		    ]
	, nest 4 $ toDoc $ formula i
	, text ""
	, parens $ vcat [ multitext [ (DE, "die Baumstruktur dieses Ausdrucks ist:" )
				    , (UK, "the expression tree is:")
				    ] 
			, nest 4 $ draw  $ formula i
			]
	, multitext [ (DE, "und nur diese Operatoren enthält:")
		    , (UK, "and which contains only these operators:")
		    ] 
	, nest 4 $ toDoc $ operators i
	]

    initial p i = formula i

    partial p i b = do
        inform $ vcat
	       [ multitext [ (DE, "Die Baumstruktur Ihrer Einsendung ist:")
			   , (UK, "The tree structure of your submission is:")
			   ]
	       , nest 4 $ draw b
	       , text ""
	       ]
        Autolib.Reporter.Set.subeq
	    ( parens $ multitext [ (DE, "benutzte Operatoren")
				 , (UK, "operators used")
				 ] , syms b )
	    ( parens $ multitext [ ( DE, "erlaubte Operatoren" )
				 , ( UK, "operators allowed" )
				 ] , operators i )
	Autolib.Reporter.Set.subeq
	    ( parens $ multitext [ (DE, "benutzte Variablen")
				 , (UK, "variables used")
				 ] , vars b )
	    ( parens $ multitext [ (DE, "Variablen der Aufgabenstellung")
				 , (UK, "variables given")
				 ] ,vars $ formula i )

    total p i b = do
        Boolean.Equiv.check ( formula i ) b



make :: Make
make = direct Boolean $  BI 
       { formula = read "x == (y == z)"
       , operators = mkSet $ read "[ false, true, !, ||, && ]"
       }



