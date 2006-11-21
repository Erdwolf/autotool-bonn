{-# OPTIONS -fglasgow-exts #-}

-- | demonstration module: how to use the Algebraic.Class

module Algebraic.Integer where

import Algebraic.Class
import Algebraic.Instance
import Data.Typeable

data Arithmetics = Arithmetics deriving ( Read, Show, Typeable )

functions :: [ Op Integer ]
functions = 
       [ Op { name = "+", arity = 2, precedence = 5, assoc = AssocLeft
            , inter = \ [ x, y ] -> x + y
            }
       , Op { name = "*", arity = 2, precedence = 7, assoc = AssocLeft
            , inter = \ [ x, y ] -> x * y
            }
       ]

zahl :: Integer -> Op Integer
zahl i = Op { name = show i, arity = 0, precedence = 0, assoc = AssocNone
            , inter = \ [ ] -> const i
            } 

instance Ops Integer where 
    ops = map zahl [ 0 .. 1 ] ++ functions

instance Algebraic Arithmetics Integer where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag exp = tfold ( error "evaluate" ) inter
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
       assert ( a == b ) 
              $ text "Werte stimmen überein?"

    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "(1 + 1)* (1 + 1)"

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = Algebraic.Instance.Make
          { target = 13
          , description = Nothing
	  , operators = ops
          , max_size = 8
          }
