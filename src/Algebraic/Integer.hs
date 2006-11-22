{-# OPTIONS -fglasgow-exts #-}

-- | demonstration module: how to use the Algebraic.Class

module Algebraic.Integer where

import qualified Autolib.TES.Binu as B

import Algebraic.Class
import Algebraic.Instance
import Condition

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Set
import Autolib.Size

import Data.Typeable

data Algebraic_Integer = Algebraic_Integer deriving ( Read, Show, Typeable )

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
    bops = B.Binu
	 { B.nullary =  map zahl [ 0 .. 1 ]	
	 , B.unary = []
	 , B.binary = functions
	 }

instance Condition () Integer where

instance Size Integer where size i = fromIntegral i

instance Algebraic Algebraic_Integer Integer where
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
	  , operators = default_operators tag
          , max_size = 7
	}

    default_operators tag = bops

