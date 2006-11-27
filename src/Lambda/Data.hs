module Lambda.Data where

import Autolib.TES.Identifier


data Lambda 
    = Variable Identifier
    | Apply Lambda Lambda
    | Abstract Identifier Lambda
    deriving ( Eq, Ord )


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


        
