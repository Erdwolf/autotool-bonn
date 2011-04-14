{-# LANGUAGE DeriveDataTypeable #-}

module Lambda.Data 

( module Lambda.Data
, Identifier
)

where

import Autolib.TES.Identifier
import Autolib.Set
import Autolib.Size
import Control.Monad ( guard )
import Data.Typeable

data Lambda 
    = Variable Identifier
    | Apply Lambda Lambda
    | Abstract Identifier Lambda
    deriving ( Eq, Ord, Typeable )

instance Size Lambda where
    size t = case t of
        Variable {} -> 1
        Apply fun arg -> size fun + size arg
        Abstract v b -> 1 + size b

free_variables :: Lambda -> Set Identifier
free_variables t = case t of
    Variable v -> unitSet v
    Apply f a -> free_variables f `union` free_variables a
    Abstract v b -> free_variables b `minusSet` unitSet v

applications :: Lambda -> ( Lambda, [ Lambda ] )
applications t = case t of
    Apply f x -> 
        let ( g, args ) = applications f
        in  ( g, args ++ [x] )
    _ -> ( t, [] )

apply :: ( Lambda, [ Lambda ] ) -> Lambda
apply ( f , args ) = foldl Apply f args

abstractions :: Lambda -> ( [ Identifier ] , Lambda )
abstractions t = case t of
    Abstract x b -> 
        let ( vars, c ) = abstractions b
        in  ( x : vars , c )
    _ -> ( [], t )

abstract :: ( [ Identifier ] , Lambda ) -> Lambda
abstract ( vars, body ) = foldr Abstract body vars

next_free vs = head $ do
    k <- [0 .. ]
    f <- "xyz" ++ "uvw" ++ "pqrs" ++ [ 'a' .. 'o' ]
    let w' = mknullary $ f : (if k == 0 then "" else show k )
    guard $ not $ elementOf w' vs
    return w'
