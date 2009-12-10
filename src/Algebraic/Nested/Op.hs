module Algebraic.Nested.Op where

import Algebraic.Nested.Type

import Expression.Op

import Autolib.ToDoc
import qualified Autolib.Set as S
import qualified Autolib.TES.Binu as B

instance Ops ( Type Integer ) where 
    bops = B.Binu
	 { B.nullary =  map zahl [ 0 .. 9 ]	
	 , B.unary = [ 
	     Op { name = "pow", arity = 1
		, precedence = Nothing, assoc = AssocNone
		, inter = lift1 power
		}
	     ]
	 , B.binary = [ 
            Op { name = "+", arity = 2, precedence = Just 5, assoc = AssocLeft
	       , inter = lift2 union
	       }
	    , Op { name = "-", arity = 2, precedence = Just 5, assoc = AssocLeft
		 , inter = lift2 difference
	      }
	    , Op { name = "&", arity = 2, precedence = Just 7, assoc = AssocLeft
		 , inter = lift2 intersection 
		 }
	    ]
	 }

zahl :: Integer -> Op ( Type Integer )
zahl i = Op { name = show i, arity = 0, precedence = Nothing, assoc = AssocNone
            , inter = lift0 $ unit i
            } 

is_empty :: Type a -> Bool
is_empty ( Make xs ) = S.isEmptySet xs

unit :: Ord a => a -> Type a 
unit x = Make $ S.mkSet [ Unit x ]

empty :: Type a
empty = Make $ S.emptySet

union :: Ord a => Type a -> Type a -> Type a 
union ( Make xs ) ( Make ys ) = Make $ S.union xs ys

difference :: Ord a => Type a -> Type a -> Type a
difference ( Make xs ) ( Make ys ) = Make $ S.minusSet xs ys

intersection :: Ord a => Type a -> Type a -> Type a
intersection ( Make xs ) ( Make ys ) = Make $ S.intersect xs ys

power :: Ord a => Type a -> Type a
power ( Make xs ) = Make $ S.mkSet $ map ( Packed . Make ) $ S.subsets xs
