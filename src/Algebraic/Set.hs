{-# OPTIONS -fglasgow-exts #-}

module Algebraic.Set where

import qualified Autolib.TES.Binu as B

import qualified Algebraic.Nested as Nested

import Algebraic.Class
import Algebraic.Instance
import Condition

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Algebraic_Set = Algebraic_Set deriving ( Read, Show, Typeable )


functions :: Ord a => [ Op ( Nested.Type a ) ]
functions = 
       [ Op { name = "+", arity = 2, precedence = Just 5, assoc = AssocLeft
            , inter = lift $ \ [ x, y ] -> Nested.union x y
            }
       , Op { name = "-", arity = 2, precedence = Just 5, assoc = AssocLeft
            , inter = lift $ \ [ x, y ] -> Nested.difference x y
            }
       , Op { name = "*", arity = 2, precedence = Just 7, assoc = AssocLeft
            , inter = lift $ \ [ x, y ] -> Nested.intersection x y
            }
       , Op { name = "pow", arity = 1, precedence = Nothing, assoc = AssocNone
            , inter = lift $ \ [ x ] -> Nested.power x 
            }
       ]

zahl :: Integer -> Op ( Nested.Type Integer )
zahl i = Op { name = show i, arity = 0, precedence = Nothing, assoc = AssocNone
            , inter = lift $ \ [ ] -> Nested.unit i
            } 

instance Ops ( Nested.Type Integer ) where 
    bops = B.Binu
	 { B.nullary =  map zahl [ 0 .. 9 ]	
	 , B.unary = []
	 , B.binary = functions
	 }

instance Condition () ( Nested.Type Integer ) where


instance Algebraic Algebraic_Set ( Nested.Type Integer ) where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag exp = do
        v <- tfoldR ( error "evaluate" ) inter exp
	return v
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
       return $ a == b

    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "pow (1 + pow (2)) "

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = Algebraic.Instance.Make
        { target = Nested.example
          , description = Nothing
	  , operators = default_operators tag
          , max_size = 7
	}

    default_operators tag = bops

