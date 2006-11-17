{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Boolean.Op 

( module Expression.Op
) 

where



import Expression.Op

import Autolib.Grah.Basic 
import Autolib.Graph.Op

import Autolib.Hash

data Graph_Or_Natural where
    Nat :: Integer -> Graph_Or_Natural
    Gra :: Graph Integer -> Graph_Or_Natural

instance Ops ( Graph_Or_Natural ) where
    ops = nullary ++ unary ++ binary 

nullary :: [ Op Graph_Or_Natural ]
nullary = do
    n <- [ 1 .. 9 ] 
    return $ Op { name = show n
                , arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = \ [ ] -> Nat n
		}

unary :: [ Op Graph_Or_Natural ]
unary = co : do
    ( tag, fun ) <- [ ( "K", clique ), ("I", independent )
                    , ( "P", path   ), ("C", circle )
                    ]
    return $ Op { name = tag 
                , arity = 1
		, precedence = Nothing , assoc = AssocNone
		, inter = \ [ Nat n ] -> Gra $ fun [ 1 .. n ]
		}
    
co =  Op { name = "co" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = wrapped $ \ [ x ] -> complement x
		}

binary :: [ Op Graph_Or_Natural ]
binary = [ times, cross, plus ]

times =  Op { name = "*" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = wrapped $ \ [x, y] -> times x y
	      }
cross =  Op { name = "%" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = wrapped $ \ [x, y] -> grid x y
	      }
plus = Op { name = "+" , arity = 2
	      , precedence = Just 7 , assoc = AssocLeft
	      , inter = wrapped \ [x, y] -> union x y
	      }

wrapped fun = Gra . fun . map ( \ ( Gra x ) -> x ) 

