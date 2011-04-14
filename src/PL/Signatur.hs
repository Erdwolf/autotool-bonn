{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module PL.Signatur where

import PL.Data
import PL.Util
import PL.ToDoc

import Autolib.FiniteMap
import qualified Data.Map as M
import Autolib.TES.Identifier
import Autolib.Set
import Autolib.Reporter
import Autolib.Reader
import Autolib.ToDoc

data Signatur =
     Signatur { funktionen :: FiniteMap Identifier Int
	      , relationen :: FiniteMap Identifier Int
	      , freie_variablen :: Set Identifier
	      }

$(derives [makeReader, makeToDoc] [''Signatur])


has_nullary_functions :: Signatur -> Bool
has_nullary_functions s = not $ null $ do
    ( f, a ) <- M.toList $ funktionen s
    guard $ 0 == a

class Signed s where 
      signatur :: s -> Signatur
      check :: Signatur -> s -> Reporter ()

instance Signed Formel where
    signatur f = case f of
        Quantified q v f -> 
            remove_variable v $ signatur f
        Predicate p ts -> 
	    add_relation (p, length ts ) $ vereinige $ map signatur ts
        Operation op fs -> 
            vereinige $ map signatur fs
	Equals l r -> 
	    vereinige $ map signatur [ l, r ]
    check sig f = case f of
        Quantified q v f -> check ( add_variable v sig ) f
	Operation op fs -> mapM_ ( check sig ) fs
        Predicate p ts -> check_exp sig ( relationen sig ) p ts

check_exp sig fm p xs = do
    n <- find_or_complain "symbol" fm p
    when ( n /= length xs ) $ reject $ vcat 
        [ text "for relation/function symbol" <+> toDoc p
        , text "declared arity" <+> toDoc n
        , text "does not match actual number of arguments" <+> toDoc ( length xs)
	, text "arguments are" <+> toDoc xs
        ]
    mapM_ ( check sig ) xs


leer :: Signatur
leer = Signatur 
            { funktionen = emptyFM
	    , relationen = emptyFM
	    , freie_variablen = emptySet
	    }

instance Signed Term where
    signatur ( Variable v ) = leer { freie_variablen = mkSet [v] }
    signatur ( Apply f xs ) = 
        add_function ( f, length xs ) $ vereinige $ map signatur xs
    check sig ( Variable v ) = do
        when ( not $ v `elementOf` freie_variablen sig ) $ reject
	     $ text "Variable" <+> toDoc v <+> text "nicht deklariert"
    check sig ( Apply f xs ) = check_exp sig ( funktionen sig ) f xs




add_function (f, n) sig = 
    sig { funktionen = addToFM ( funktionen sig ) f n }

add_relation (r, n) sig =
    sig { relationen = addToFM ( relationen sig ) r n }

vereinige = foldr vereinige_zwei leer

vereinige_zwei s t = Signatur
    { funktionen = plusFM ( funktionen s ) ( funktionen t )
    , relationen = plusFM ( relationen s ) ( relationen t )
    , freie_variablen = union ( freie_variablen s ) ( freie_variablen t )
    }

remove_variable v sig = 
    sig { freie_variablen = minusSet ( freie_variablen sig ) ( unitSet v ) }

add_variable v sig = 
    sig { freie_variablen = union ( freie_variablen sig ) ( unitSet v ) }

