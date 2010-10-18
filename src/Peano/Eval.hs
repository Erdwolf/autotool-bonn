module Peano.Eval where

import qualified Peano.Type as T
import qualified Peano.Data as D

import Autolib.Reporter
import Autolib.ToDoc

-- | the type in ValFun is just the argument type 
-- (not the result type)
data Val = ValNat Integer
         | ValFun ( Val -> Reporter Val )

instance ToDoc Val where
    toDoc v = case v of
        ValNat n -> toDoc n
        ValFun _ -> text "<<<function>>>"

type Env = String -> Maybe Val

std :: Env
std "s" = Just $ ValFun $ \ v -> 
    with_nat ( return v ) $ \ n -> return $ ValNat $ succ n 
std "z" = Just $ ValNat 0 
std _ = Nothing

value :: Env -> D.Exp -> Reporter Val
value env x = case x of
    D.Const i -> return $ ValNat i
    D.Ref n -> case env n of
        Just v -> return v
        Nothing -> reject $ hsep [ text "Name", text n, text "nicht gebunden" ]
    D.Abs n t x -> 
        return $ ValFun $ \ v -> do
            value ( extend n v env ) x
    D.App f a -> 
        with_fun ( value env f ) $ \ f -> 
        with_val ( value env a ) $ \ a -> 
        f a
    D.Fold z s -> 
        with_val ( value env z ) $ \ z -> 
        with_fun ( value env s ) $ \ s -> 
        return $ ValFun $ \ v -> 
            with_nat ( return v ) $ \ n -> fold z s n

fold :: Monad m
     => a -> ( a -> m a ) -> Integer -> m a
fold z s n = 
    if n > 0 
    then fold z s ( pred n ) >>= s
    else return z

extend n v env = 
    \ m -> if n == m then Just v else env m

with_val :: Reporter Val
         -> ( Val -> Reporter Val )
         -> Reporter Val
with_val v k = do
    x <- v
    k x

with_nat :: Reporter Val
         -> ( Integer -> Reporter Val )
         -> Reporter Val
with_nat v k = do
    x <- v
    case x of
        ValNat n -> k n
        _ -> reject $ text "Zahl erwartet"

with_fun :: Reporter Val 
         -> ( ( Val -> Reporter Val ) -> Reporter Val ) 
         -> Reporter Val
with_fun v k = do
    x <- v
    case x of
        ValFun f -> k f
        _ -> reject $ text "Funktion erwartet"


    