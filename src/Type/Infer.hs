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
    inform $ text "Berechne Typ für Ausdruck:" <+> toDoc exp
    t <- nested 4 $ case exp of
        Node n [] ->
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    inform $ text "Variable" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc v
                    return $ vtype v
                [   ] -> reject $ text "Variable" <+> toDoc n <+> text "ist nicht deklariert."
                vs -> reject $ vcat
                         [ text "Variable" <+> toDoc n <+> text "ist mehrfach deklariert:"
                         , toDoc vs
                         ]
        Node n [arg] | show n == "&" -> do
            assert ( isVariable arg )
                   $ text "Ist Variable?"
            t <- nested 4 $ infer sig arg
            return $ PointerTo t
        Node n [arg] | show n == "*" -> do
            t <- nested 4 $ infer sig arg
            assert ( isPointerType t )
                   $ text "Ist Pointer-Typ?"
            let PointerTo t' = t
            return t'
        Node n args ->
            case [ f | f <- functions sig, fname f == n ]
            of  [   ] -> reject $ text "Funktion" <+> toDoc n <+> text "ist nicht deklariert."
                fs    ->
                  case [ f | f <- fs, length args == length (arguments f) ]
                  of  [   ] -> reject $ vcat [ text "Funktion" <+> toDoc n <+> text "hat keine Deklaration mit der richtigen Anzahl an Argumenten:"
                                             , toDoc fs
                                             ]
                      [ f ] -> do
                          inform $ text "Funktion" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc f
                          sequence_ $ do
                              ( k, arg ) <- zip [1..] args
                              return $ do
                                  inform $ text "Prüfe Argument Nr." <+> toDoc k
                                  t <- nested 4 $ infer sig arg
                                  assert ( t == arguments f !! (k-1) )
                                         $ text "Argument-Typ stimmt mit Deklaration überein?"
                          return $ result f
                      fs    -> reject $ vcat
                               [ text "Funktion" <+> toDoc n <+> text "ist mehrfach deklariert:"
                               , toDoc fs
                               ]
    inform $ text "Ausdruck" <+> toDoc exp <+> text "hat Typ:" <+> toDoc t
    return t

--isVariable :: Exp -> Bool
isVariable (Node _ []) = True
isVariable _           = False

