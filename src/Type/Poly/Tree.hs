-- | TODO: move to Autolib.*

module Type.Poly.Tree 

( peng 
)

where

import Type.Poly.Data

import qualified Tree as T

import Autolib.Dot.Dotty

instance T.ToTree Type where
    toTree ( TyVar v ) = 
        T.Node ( show v ) []
    toTree ( TyCon f args ) = 
        T.Node ( show f ) $ map T.toTree args

instance T.ToTree Expression where
    toTree ( Apply ts f args ) = 
        T.Node ( show f ) $ map T.toTree ts ++ map T.toTree args
