{-# language TemplateHaskell #-}

module CL.Combinator where

import CL.Term

import qualified Autolib.Reader as R
import qualified Autolib.ToDoc  as T

import Control.Monad ( when )
import Data.List ( nub )

data Combinator = 
     Combinator { name :: Identifier
                , arguments :: [ Identifier ]
                , result :: Term
                }

arity c = length $ arguments c

instance T.ToDoc Combinator where
    toDoc c = T.hsep 
          [ T.toDoc ( unspine $ map Sym $ name c : arguments c )
          , T.text "->"
          , T.toDoc $ result c
          ]
           
instance R.Reader Combinator where
    reader = do
        lhs <- R.reader
        when ( not $ all isSym $ spine lhs ) 
             $ fail "all combinator arguments must be symbols"
        let xs = map unSym $ spine lhs
        when ( xs /= nub xs ) 
             $ fail "all combinator arguments must be distinct"
        R.my_symbol "->"
        rhs <- R.reader
        return $ Combinator { name = head xs , arguments = tail xs, result = rhs }

standard_base_for_cl :: [ Combinator ]
standard_base_for_cl =
    [ read "S x y z -> x z (y z)" 
    , read "K x y -> x"
    , read "I x -> x"
    ]

standard_base_for_cli :: [ Combinator ]
standard_base_for_cli = 
    [ read "S x y z -> x z (y z)" 
    , read "B x y z -> x z y"
    , read "C x y z -> x (y z)"
    , read "I x -> x"
    ]
