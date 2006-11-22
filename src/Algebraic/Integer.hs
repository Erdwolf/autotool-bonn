{-# OPTIONS -fglasgow-exts #-}

-- | demonstration module: how to use the Algebraic.Class

module Algebraic.Integer where

import Algebraic.Class
import Algebraic.Instance
import Data.Typeable

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Set

data Tag = Tag deriving ( Read, Show, Typeable )

functions :: [ Op Integer ]
functions = 
       [ Op { name = "+", arity = 2, precedence = Just 5, assoc = AssocLeft
            , inter = lift $ \ [ x, y ] -> x + y
            }
       , Op { name = "*", arity = 2, precedence = Just 7, assoc = AssocLeft
            , inter = lift $ \ [ x, y ] -> x * y
            }
       ]

zahl :: Integer -> Op Integer
zahl i = Op { name = show i, arity = 0, precedence = Nothing, assoc = AssocNone
            , inter = lift $ \ [ ] -> i
            } 

instance Ops Integer where 
    ops = map zahl [ 0 .. 1 ] ++ functions

instance Algebraic Tag Integer where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag exp = do
        v <- tfoldR ( error "evaluate" ) inter exp
	return v
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
       return $ a == b

    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "(1 + 1)* (1 + 1)"

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = Algebraic.Instance.Make
          { target = 13
          , description = Nothing
	  , operators = mkSet ops
          , max_size = 8
          }
