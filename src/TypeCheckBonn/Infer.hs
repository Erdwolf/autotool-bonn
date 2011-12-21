module TypeCheckBonn.Infer where

import Type.Data
import Type.Tree

import Autolib.Reporter.Type hiding ( result )
import Autolib.ToDoc

import Autolib.TES.Term
import Autolib.TES.Identifier

import Control.Monad.Writer

type Exp = Term Identifier Identifier

infer :: Signature -> Exp -> Writer Doc (Maybe Type)
infer sig exp = do
    tell $ text "Berechne Typ für Ausdruck:" <+> toDoc exp
    t <- nested 4 $ case exp of
        Node n [] ->
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    tell $ text "Variable" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc v
                    return $ vtype v
                [   ] -> do
                   reject $ text "Variable" <+> toDoc n <+> text "ist nicht deklariert."
                   mzero
                vs -> do
                   tell $ vcat
                      [ text "Variable" <+> toDoc n <+> text "ist mehrfach deklariert:"
                      , toDoc vs
                      ]
                   mzero
        Node n args ->
            case [ f | f <- functions sig, fname f == n ]
            of  [   ] -> reject $ text "Funktion" <+> toDoc n <+> text "ist nicht deklariert."
                fs    ->
                  case [ f | f <- fs, length args == length (arguments f) ]
                  of  [   ] -> reject $ vcat [ text "Funktion" <+> toDoc n <+> text "hat keine Deklaration mit der richtigen Anzahl an Argumenten:"
                                             , toDoc fs
                                             ]
                      [ f ] -> do
                          tell $ text "Funktion" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc f
                          zip [1..] args `forM_` \( k, arg ) -> do
                              let paramType = arguments f !! (k-1)
                              tell $ text "Prüfe Argument Nr." <+> toDoc k
                              t <- nested 4 $ infer sig arg
                              assert ( t == paramType )
                                      $ text "Argument-Typ stimmt mit Deklaration überein?"
                              mzero
                          return $ result f
                      fs    -> tell $ vcat
                               [ text "Funktion" <+> toDoc n <+> text "ist mehrfach deklariert:"
                               , toDoc fs
                               ]
                               mzero
    tell $ text "Ausdruck" <+> toDoc exp <+> text "hat Typ:" <+> toDoc t
    return t

isVariable :: Exp -> Bool
isVariable (Node _ []) = True
isVariable _           = False

{-
infer :: Signature -> Type -> Exp -> Writer Doc ()
infer sig goal exp = do
    tell $ text "Prüfe, ob der Ausdruck " <+> toDoc exp <+> text "den Typ" <+> toDoc goal <+> text "hat:"
    nested 4 $ case exp of
        Node n [] ->
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    tell $ text "Variable" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc v
                    assert (vtype v == goal) $ text "Richtiger Typ?"
                    return ()
                [   ] -> reject $ text "Variable" <+> toDoc n <+> text "ist nicht deklariert."
                vs -> reject $ vcat
                         [ text "Variable" <+> toDoc n <+> text "ist mehrfach deklariert:"
                         , toDoc vs
                         ]
        Node n args ->
            case [ f | f <- functions sig, fname f == n ]
            of  [   ] -> reject $ text "Funktion" <+> toDoc n <+> text "ist nicht deklariert."
                fs    ->
                  case [ f | f <- fs, length args == length (arguments f) ]
                  of  [   ] -> reject $ vcat [ text "Funktion" <+> toDoc n <+> text "hat keine Deklaration mit der richtigen Anzahl an Argumenten:"
                                             , toDoc fs
                                             ]
                      [ f ] -> do
                          tell $ text "Funktion" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc f
                          assert (result f == goal) $ text "Richtiger Rückgabe-Typ?"
                          zip [1..] args `forM_` \( k, arg ) -> do
                              let paramType = arguments f !! (k-1)
                              tell $ text "Prüfe Argument Nr." <+> toDoc k
                              nested 4 $ infer sig paramType arg

                      fs    -> reject $ vcat
                               [ text "Funktion" <+> toDoc n <+> text "ist mehrfach deklariert:"
                               , toDoc fs
                               ]
    tell $ text "Ja, Ausdruck" <+> toDoc exp <+> text "hat Typ:" <+> toDoc goal

-}
