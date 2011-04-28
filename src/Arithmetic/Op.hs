{-# LANGUAGE UndecidableInstances, FlexibleInstances #-}
module Arithmetic.Op 

( Exp, eval, bind
)

where

--  $Id$

import Expression.Op

import Autolib.TES.Identifier
import Autolib.TES.Term (tfold)
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Prime (prime)

import Data.Char ( isDigit )
import Control.Monad ( guard )

instance (Enum a, Num a, Integral a ) => Ops a where
    ops = nullary ++ unary ++ binary


nullary :: ( Enum a, Num a ) => [ Op a ]
nullary = do
    guard $ False -- das machen wir anders
    i <- [ 0 .. 9 ]
    return $ Op { name = show i, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = lift $ \ [] -> i
		}

unary :: ( Num a , Integral a ) =>  [ Op a ]
unary = [ Op { name = "negate" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] -> negate x
	     }
        , Op { name = "abs" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] -> abs x
	     }
	, Op { name = "fib" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
	               in lift $ \ [x] -> fibs !! (fromIntegral x)
	     }
	, Op { name = "sqrt" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] ->
                  floor $ (sqrt (fromIntegral x) :: Double)
	     }
	, Op { name = "log2" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] ->
                  floor $ (logBase 2 (fromIntegral x) :: Double)
	     }
	, Op { name = "fac" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] -> product [1..(fromIntegral x)]
	     }
	, Op { name = "is_prime" , arity = 1
	     , precedence = Just 10 , assoc = AssocNone
	     , inter = lift $ \ [x] -> if prime (fromIntegral x) then 1 else 0
	     }
	]

binary :: ( Num a , Integral a ) =>  [ Op a ]
binary = [ Op { name = "*" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> x * y
	      } 
	 , Op { name = "/" , arity = 2
	      , precedence = Just 8, assoc = AssocLeft
	      , inter = lift $ \ [x,y] -> if y == 0 then 0 else div x y
	      }
	 , Op { name = "`div`" , arity = 2
	      , precedence = Just 8, assoc = AssocLeft
	      , inter = lift $ \ [x,y] -> if y == 0 then 0 else div x y
	      }
	 , Op { name = "%" , arity = 2
	      , precedence = Just 4, assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> if y == 0 then 0 else mod x y
	      }
	 , Op { name = "`mod`" , arity = 2
	      , precedence = Just 4, assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> if y == 0 then 0 else mod x y
	      }
	 , Op { name = "^" , arity = 2
	      , precedence = Just 2, assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> x ^ y
	      }
	 , Op { name = "`pow`" , arity = 2
	      , precedence = Just 2, assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> x ^ y
	      }
         , Op { name = "+" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> x + y
	      } 
         , Op { name = "-" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> x - y
	      } 
         , Op { name = ":-" , arity = 2
	      , precedence = Just 6 , assoc = AssocLeft
	      , inter = lift $ \ [x, y] -> max 0 $ x - y
	      } 
         , Op { name = "`min`" , arity = 2
	      , precedence = Just 0 , assoc = AssocNone
	      , inter = lift $ \ [x, y] -> min x y
	      } 
         , Op { name = "`max`" , arity = 2
	      , precedence = Just 0 , assoc = AssocNone
	      , inter = lift $ \ [x, y] -> max x y
	      } 
	 ]

eval :: FiniteMap Identifier Integer 
     -> Exp Integer 
     -> Reporter Integer
eval fm =  
    let find b = case lookupFM fm b of
          Just i -> return i
          Nothing | all isDigit $ show b -> return $ read $ show b
          Nothing -> reject $ text $ "Arithmetic.Op.eval: " ++ show b 
    in tfoldR ( find ) ( inter )

bind :: [ (String, Integer) ] -> FiniteMap Identifier Integer
bind xvs = listToFM $ do (x, v) <- xvs ; return ( mknullary x, v )

