module Type.Infer where

--  $Id$

import Type.Data
import Type.Tree

import Autolib.Reporter.Type hiding ( result )
import Autolib.ToDoc

import Autolib.TES.Term
import Autolib.TES.Identifier

import Control.Monad ( guard )

type Exp = Term Identifier Identifier

infer :: Signature -> Exp -> Reporter Type
infer sig exp = do
    inform $ text "berechne Typ für Ausdruck:" <+> toDoc exp
    t <- nested 4 $ case exp of
	Node n [] ->
            case do v <- variables sig ; guard $ vname v == n ; return v
            of  [ v ] -> do
                    inform $ text "ist Variable mit Deklaration:" <+> toDoc v
		    return $ vtype v
	        [   ] -> reject $ text "ist nicht deklarierte Variable."
		vs -> reject $ vcat
		         [ text "ist mehrfach deklarierte Variable:"
			 , toDoc vs
			 ]
        Node n args -> 
	    case do f <- functions sig ; guard $ fname f == n ; return f
	    of  [ f ] -> do
		    inform $ text "Funktion hat Deklaration:" <+> toDoc f
		    assert ( length args == length ( arguments f ) )
			   $ text "Anzahl der Argumente stimmt mit Deklaration überein?" 
		    sequence_ $ do
		        ( k, arg ) <- zip [1..] args
                        return $ do
                            inform $ text "prüfe Argument Nr." <+> toDoc k
			    t <- nested 4 $ infer sig arg
			    assert ( t == arguments f !! (k-1) )
				   $ text "Argument-Typ stimmt mit Deklaration überein?"
		    return $ result f
                [   ] -> reject $ text "ist nicht deklarierte Funktion."
		fs    -> reject $ vcat
		         [ text "ist mehrfach deklarierte Funktion:"
			 , toDoc fs
			 ]
    inform $ text "hat Typ:" <+> toDoc t
    return t

