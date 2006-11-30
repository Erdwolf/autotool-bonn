module Lambda.Alpha where

import Lambda.Data
import Autolib.Set

convertible :: Lambda -> Lambda -> Bool
convertible ( Variable v1 ) ( Variable v2 ) = 
    v1 == v2
convertible ( Apply f1 a1 ) ( Apply f2 a2 ) = 
    convertible f1 f2 && convertible a1 a2
convertible ( Abstract v1 b1 ) ( Abstract v2 b2 ) = 
    let v = next_free $ free_variables b1 `union` free_variables b2
    in  convertible ( free_rename v1 v b1 ) ( free_rename v2 v b2 )
convertible _ _ = False

-- | replace each free occurence of v by a,
-- rename bound variables in b when necessary.
-- implementation is not efficient (will compute FV(t) repeatedly)
free_sub :: Identifier -> Lambda -> Lambda -> Lambda
free_sub v a t = case t of
    Variable w -> if v == w then a else t
    Apply fun arg -> Apply ( free_sub v a fun ) ( free_sub v a arg )
    Abstract w b -> 
        let ( w', b' ) = if w `elementOf` free_variables t
                         then let w' = next_free ( free_variables t )
                              in  ( w', free_rename w w' b )
                         else ( w, b )
        in  Abstract w' $ free_sub v a b'


-- | rename all free occurences of first argument by second argument in third
free_rename :: Identifier -> Identifier -> Lambda -> Lambda
free_rename v w t = case t of
    Variable u -> if v == u then Variable w else t
    Apply fun arg -> Apply (free_rename v w fun) ( free_rename v w arg)
    Abstract u b -> 
        if v == u then t else Abstract u $ free_rename v w b
