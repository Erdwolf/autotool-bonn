module Arithmetic.Op 

( Exp, eval, bind
)

where

--  $Id$

import Expression.Op
import Autolib.TES.Identifier
import Autolib.TES.Term (tfold)
import Autolib.FiniteMap
import Autolib.Prime (prime)


instance (Enum a, Num a, Integral a ) => Ops a where
    ops = nullary ++ unary ++ binary

nullary :: ( Enum a, Num a ) => [ Op a ]
nullary = do
    i <- [ 0 .. 9 ]
    return $ Op { name = show i, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = \ [] -> i
		}

unary :: ( Num a , Integral a ) =>  [ Op a ]
unary = [ Op { name = "negate" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = \ [x] -> negate x
	     }
	, Op { name = "fib" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
	               in \ [x] -> fibs !! (fromIntegral x)
	     }
	, Op { name = "sqrt" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = \ [x] ->
                  floor $ (sqrt (fromIntegral x) :: Double)
	     }
	, Op { name = "fac" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = \ [x] -> product [1..(fromIntegral x)]
	     }
	, Op { name = "is_prime" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = \ [x] -> if prime (fromIntegral x) then 1 else 0
	     }
	]

binary :: ( Num a , Integral a ) =>  [ Op a ]
binary = [ Op { name = "*" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = \ [x, y] -> x * y
	      } 
	 , Op { name = "/" , arity = 2
	      , precedence = Just 8, assoc = AssocLeft
	      , inter = \ [x,y] -> if y == 0 then 0 else div x y
	      }
	 , Op { name = "`div`" , arity = 2
	      , precedence = Just 8, assoc = AssocLeft
	      , inter = \ [x,y] -> if y == 0 then 0 else div x y
	      }
	 , Op { name = "%" , arity = 2
	      , precedence = Just 4, assoc = AssocLeft
	      , inter = \ [x, y] -> if y == 0 then 0 else mod x y
	      }
	 , Op { name = "`mod`" , arity = 2
	      , precedence = Just 4, assoc = AssocLeft
	      , inter = \ [x, y] -> if y == 0 then 0 else mod x y
	      }
	 , Op { name = "^" , arity = 2
	      , precedence = Just 2, assoc = AssocLeft
	      , inter = \ [x, y] -> x ^ y
	      }
	 , Op { name = "`pow`" , arity = 2
	      , precedence = Just 2, assoc = AssocLeft
	      , inter = \ [x, y] -> x ^ y
	      }
         , Op { name = "+" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = \ [x, y] -> x + y
	      } 
         , Op { name = "-" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = \ [x, y] -> x - y
	      } 
         , Op { name = ":-" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = \ [x, y] -> max 0 $ x - y
	      } 
         , Op { name = "`min`" , arity = 2
	      , precedence = Just 0 , assoc = AssocNone
	      , inter = \ [x, y] -> min x y
	      } 
         , Op { name = "`max`" , arity = 2
	      , precedence = Just 0 , assoc = AssocNone
	      , inter = \ [x, y] -> max x y
	      } 
	 ]

eval :: FiniteMap Identifier Integer -> Exp Integer -> Integer
eval b =  tfold ( lookupWithDefaultFM b (error "Arithmetic.Op.eval") )
          ( inter )


bind :: [ (String, Integer) ] -> FiniteMap Identifier Integer
bind xvs = listToFM $ do (x, v) <- xvs ; return ( mknullary x, v )
