module Peano.Infer where

import qualified Peano.Type as T
import qualified Peano.Data as D

import Autolib.Reporter
import Autolib.ToDoc

type Env = String -> Maybe T.Type

std :: Env
std "s" = Just $ T.App T.Nat T.Nat
std "z" = Just $ T.Nat
std _ = Nothing

typeof :: Env -> D.Exp -> Reporter T.Type
typeof env x = do
    inform $ toDoc x <+> text "::" <+> text "?"
    t <- nested 4 $ typeof_ env x
    inform $ toDoc x <+> text "::" <+> toDoc t
    return t

typeof_ env x = case x of
    D.Const n -> return T.Nat
    D.Ref n -> case env n of
        Just v -> return v
        Nothing -> reject $ hsep [ text "Name", text n, text "nicht gebunden" ]
    D.Abs n t b -> do
        b <- typeof ( extend n t env ) b
        return $ T.App t b
    D.App f a -> 
        with_app ( typeof env f ) $ \ arg res -> 
        with_val ( typeof env a ) $ \ a -> do
            when ( arg /= a ) $ reject $ text "Argumenttyp" </> vcat
                 [ text "erwartet:" </> toDoc arg
                 , text "erhalten:" </> toDoc a
                 ]
            return res
    D.Fold z s -> 
        with_val ( typeof env z ) $ \ z -> 
        with_app ( typeof env s ) $ \ arg res -> do
            when ( z /= arg ) $ reject 
                 $ text "fold-Typschema paßt nicht (z /= arg)"
            when ( z /= res ) $ reject 
                 $ text "fold-Typschema paßt nicht (z /= res)"
            return $ T.App T.Nat z

extend n v env = 
    \ m -> if n == m then Just v else env m

with_val :: Reporter T.Type
         -> ( T.Type -> Reporter T.Type )
         -> Reporter T.Type
with_val v k = do
    x <- v
    k x

with_app :: Reporter T.Type 
         -> ( T.Type -> T.Type -> Reporter T.Type ) 
         -> Reporter T.Type
with_app v k = do
    x <- v
    case x of
        T.App arg res -> k arg res
        _ -> reject $ text "Funktionstyp erwartet"


    