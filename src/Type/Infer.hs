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
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    inform $ text "ist Variable mit Deklaration:" <+> toDoc v
                    return $ vtype v
                [   ] -> reject $ text "ist nicht deklarierte Variable."
                vs -> reject $ vcat
                         [ text "ist mehrfach deklarierte Variable:"
                         , toDoc vs
                         ]
        Node n args ->
            case [ f | f <- functions sig ++ builtins, fname f == n ]
            of  [   ] -> reject $ text "ist nicht deklarierte Funktion."
                fs    ->
                  case [ f | f <- fs, length args == length (arguments f) ]
                  of  [   ] -> reject $ vcat [ text "hat keine Deklaration mit der richtigen Anzahl an Argumenten:"
                                             , toDoc fs
                                             ]
                      [ f ] -> do
                          inform $ text "Funktion hat Deklaration:" <+> toDoc f
                          sequence_ $ do
                              ( k, arg ) <- zip [1..] args
                              return $ do
                                  inform $ text "prüfe Argument Nr." <+> toDoc k
                                  t <- nested 4 $ infer sig arg
                                  assert ( t == arguments f !! (k-1) )
                                         $ text "Argument-Typ stimmt mit Deklaration überein?"
                          return $ result f
                      fs    -> reject $ vcat
                               [ text "ist mehrfach deklarierte Funktion:"
                               , toDoc fs
                               ]
    inform $ text "hat Typ:" <+> toDoc t
    return t

builtins :: [Function]
builtins = []
