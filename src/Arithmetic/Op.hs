module Arithmetic.Op 

( Exp, eval, bind
)

where

--  $Id$

import Expression.Op
import Autolib.TES.Identifier
import Autolib.TES.Term (Term, tfold)
import Autolib.FiniteMap


instance (Enum a, Num a ) => Ops a where
    ops = nullary ++ unary ++ binary

nullary :: ( Enum a, Num a ) => [ Op a ]



nullary = do
    i <- [ 0 .. 9 ]
    return $ Op { name = show i, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = \ [] -> i
		}

unary :: Num a =>  [ Op a ]
unary = do
    return $ Op { name = "negate" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = \ [x] -> negate x
		}

binary :: Num a =>  [ Op a ]
binary = [ Op { name = "*" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = \ [x, y] -> x * y
	      } 
         , Op { name = "+" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = \ [x, y] -> x + y
	      } 
         , Op { name = "-" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = \ [x, y] -> x - y
	      } 
	 ]

eval :: FiniteMap Identifier Integer -> Exp Integer -> Integer
eval b =  tfold ( lookupWithDefaultFM b (error "Arithmetic.Op.eval") )
          ( inter )


bind :: [ (String, Integer) ] -> FiniteMap Identifier Integer
bind xvs = listToFM $ do (x, v) <- xvs ; return ( mknullary x, v )

