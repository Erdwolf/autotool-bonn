module TypeCheckBonn.Infer where

import Type.Data
import Type.Tree

import Autolib.Reporter.Type hiding ( result )
import Autolib.ToDoc

import Autolib.TES.Term
import Autolib.TES.Identifier

import Control.Monad ( guard )

type Exp = Term Identifier Identifier

infer :: Signature -> Type -> Exp -> Reporter Type
infer sig goal exp = do
    inform $ text "Prüfe, ob der Ausdruck " <+> toDoc exp <+> text "den Typ" <+> toDoc goal <+> text "hat."
    t <- nested 4 $ case exp of
        Node n [] ->
            case [ v | v <- variables sig, vname v == n ]
            of  [ v ] -> do
                    inform $ text "Variable" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc v
                    assert (vtype v == goal) $ text "Richtiger Typ?"
                    return $ vtype v
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
                          inform $ text "Funktion" <+> toDoc n <+> text "hat Deklaration:" <+> toDoc f
                          assert (result f == goal) $ text "Richtiger Typ?"
                          assert ( t == paramType )
                                         $ text "Argument-Typ stimmt mit Deklaration überein?"
                          sequence_ $ do
                              ( k, arg ) <- zip [1..] args
                              return $ do
                                  let paramType = arguments f !! (k-1)
                                  inform $ text "Prüfe Argument Nr." <+> toDoc k
                                  nested 4 $ infer sig paramType arg
                          return $ result f
                      fs    -> reject $ vcat
                               [ text "Funktion" <+> toDoc n <+> text "ist mehrfach deklariert:"
                               , toDoc fs
                               ]
    inform $ text "Ja, Ausdruck" <+> toDoc exp <+> text "hat Typ:" <+> toDoc t
    return t

isVariable :: Exp -> Bool
isVariable (Node _ []) = True
isVariable _           = False
