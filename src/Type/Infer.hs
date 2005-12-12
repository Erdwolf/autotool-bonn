module Type.Infer where

--  $Id$

import Type.Data
import Type.Tree
import Type.Lookup

import Autolib.Reporter.Type hiding ( result )
import Autolib.ToDoc

import Autolib.TES.Term
import Autolib.TES.Identifier

import Control.Monad ( guard, when )

type Exp = Term Qualified_Name Qualified_Name

infer :: Signature -> Exp -> Reporter Type
infer sig ( Var v ) = error "Type.Infer: Var" -- darf nicht vorkommen
infer sig exp @ ( Node n args ) = do
    inform $ text "berechne Typ für Ausdruck:" <+> toDoc exp
    tags <- look_deep sig sig n
    nested 2 $ do
        inform $ text "der Name" <+> toDoc n <+> text "..."
        nested 2 $ case tags of
            [ Class_Tag c ] -> reject $ text "bezeichnet eine Klasse"
            [ Method_Tag m ] -> infer_method sig m args
            [ Variable_Tag v ] -> infer_variable sig v args
            [] -> reject $ text "ist nicht deklariert"
            tags -> reject $ text "ist mehrfach deklariert"

infer_variable sig v args = do
    inform $ text "bezeichnet die Variable mit Deklaration:" <+> toDoc v
    when ( not $ null args ) $ reject 
         $ text "eine Variable darf keine Argumentliste haben"
    return $ vtype v

infer_method sig m args = do
    inform $ text "Methode hat Deklaration:" <+> toDoc m
    assert ( length args == length ( arguments m ) )
	   $ text "Anzahl der Argumente stimmt mit Deklaration überein?" 
    sequence_ $ do
        ( k, arg ) <- zip [1..] args
        return $ do
           inform $ text "prüfe Argument Nr." <+> toDoc k
           t <- nested 4 $ infer sig arg
	   assert ( t == arguments m !! (k-1) )
                  $ text "Argument-Typ stimmt mit Deklaration überein?"
    inform $ text "Resultat hat Typ:" <+> toDoc ( result m )
    return $ result m

