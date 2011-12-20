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

infer :: Symbol a => Signature a -> Term a a -> Reporter Type
infer sig exp = do
    inform $ text "Berechne Typ für Ausdruck:" <+> toDoc exp
    t <- nested 4 $ case exp of
        Node n [] ->
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    inform $ text "Ist Variable mit Deklaration:" <+> toDoc v
                    return $ vtype v
                [   ] -> reject $ text "Ist nicht deklarierte Variable."
                vs -> reject $ vcat
                         [ text "Ist mehrfach deklarierte Variable:"
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
            of  [   ] -> reject $ text "Ist nicht deklarierte Funktion."
                fs    ->
                  case [ f | f <- fs, length args == length (arguments f) ]
                  of  [   ] -> reject $ vcat [ text "Hat keine Deklaration mit der richtigen Anzahl an Argumenten:"
                                             , toDoc fs
                                             ]
                      [ f ] -> do
                          inform $ text "Funktion hat Deklaration:" <+> toDoc f
                          sequence_ $ do
                              ( k, arg ) <- zip [1..] args
                              return $ do
                                  inform $ text "Prüfe Argument Nr." <+> toDoc k
                                  t <- nested 4 $ infer sig arg
                                  assert ( t == arguments f !! (k-1) )
                                         $ text "Argument-Typ stimmt mit Deklaration überein?"
                          return $ result f
                      fs    -> reject $ vcat
                               [ text "Ist mehrfach deklarierte Funktion:"
                               , toDoc fs
                               ]
    inform $ text "Hat Typ:" <+> toDoc t
    return t

--isVariable :: Exp -> Bool
isVariable (Node _ []) = True
isVariable _           = False

