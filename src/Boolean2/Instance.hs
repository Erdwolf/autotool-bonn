-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Boolean2.Instance where

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

import Autolib.TES.Type 
import Autolib.TES.Position ( subterms )

import Inter.Types hiding ( Var )

data Boolean2 = Boolean2 deriving ( Eq, Ord, Typeable )

instance OrderScore Boolean2 where
    scoringOrder _ = Increasing

$(derives [makeReader, makeToDoc] [''Boolean2])

data Constraint 
    = Operators [ Op Bool ] 
    | Normalform Form

data Form 
    = Negationstechnisch
    | Konjunktiv
    | Disjunktiv
    --  | Kanonisch

$(derives [makeReader, makeToDoc] [''Constraint])

$(derives [makeReader, makeToDoc] [''Form])

data BI =
     BI { formula :: Exp Bool
	, constraints :: [ Constraint ]
	}
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''BI])

data BIC =
     BIC { formula_size :: Int
	 , operators_in_instance :: B.Binu ( Op Bool )
	 , constraints_for_solution :: [ Constraint ]
	 }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''BIC])

instance Partial Boolean2 BI ( Exp Bool ) where

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
	, multitext [ (DE, "und diese Bedingungen erfüllt:")
		    , (UK, "and fulfills these constraints:")
		    ] 
	, nest 4 $ toDoc $ constraints i
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
	    ( parens $ multitext [ (DE, "benutzte Variablen")
				 , (UK, "variables used")
				 ] , vars b )
	    ( parens $ multitext [ (DE, "Variablen der Aufgabenstellung")
				 , (UK, "variables given")
				 ] ,vars $ formula i )
        forM_ ( constraints i ) $ check b

    total p i b = do
        Boolean.Equiv.check ( formula i ) b


check b c = case c of
    Operators ops ->
        Autolib.Reporter.Set.subeq
	    ( parens $ multitext [ (DE, "benutzte Operatoren")
				 , (UK, "operators used")
				 ] , syms b )
	    ( parens $ multitext [ ( DE, "erlaubte Operatoren" )
				 , ( UK, "operators allowed" )
				 ] , mkSet ops )
    Normalform f -> case f of
        Negationstechnisch -> do
            inform $ text "prüfe negationstechnische Normalform"
            subcheck "!"  [] b
        Konjunktiv -> do 
            inform $ text "prüfe konjunktive Normalform"
            subcheck "&&" [ "&&", "||", "!" ] b
            subcheck "||" [ "||", "!" ] b
            subcheck "!"  [] b
        Disjunktiv -> do 
            inform $ text "prüfe disjunktive Normalform"
            subcheck "||" [ "||", "&&", "!" ] b 
            subcheck "&&" [ "&&", "!" ] b
            subcheck "!"  [] b

subcheck parent children b = forM_ ( subterms b ) $ \ s -> case s of
    Node f ts | name f == parent -> forM_ ts $ \ t ->  case t of
        Node g _ -> when ( not ( name g `elem` children )) $ reject $ vcat 
                         [ text "Kind von" <+> toDoc parent <+> text "darf nur" 
                          <+> ( if null children then empty
                               else toDoc children <+> text "oder"
                              )
                          <+> text "Variable sein"
                         , nest 4 $ toDoc s
                         ]
        _ -> return ()
    _ -> return ()

make :: Make
make = direct Boolean2 $  BI 
       { formula = read "x == (y == z)"
       , constraints = [ Operators $ read "[ false, true, !, ||, && ]" 
                       ]
       }



