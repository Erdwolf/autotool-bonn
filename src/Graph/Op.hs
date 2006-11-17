{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Graph.Op 

( module Expression.Op
, eval
) 

where



import Expression.Op

import Autolib.Graph.Graph ( Graph )
import qualified Autolib.Graph.Basic as B
import qualified Autolib.Graph.Ops as O
import qualified Autolib.Graph.Line as L

import Autolib.Hash
import Autolib.Set ( mkSet )
import Autolib.FiniteMap

eval0 = eval emptyFM
eval b = tfold ( lookupWithDefaultFM b $ error "Graph.Op.eval" ) 
               ( inter )

data Graph_Or_Natural where
    Nat :: Int -> Graph_Or_Natural
    Gra :: Graph Int -> Graph_Or_Natural

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
unary = [ co, line ] : do
    ( tag, fun ) <- [ ( "K", B.clique . mkSet )
		    , ("I", B.independent . mkSet )
                    , ( "P", B.path   ), ("C", B.circle )
                    ]
    return $ Op { name = tag 
                , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = \ [ Nat n ] -> Gra $ fun [ 1 .. n ]
		}
    
co =  Op { name = "co" , arity = 1
		, precedence = Nothing , assoc = AssocNone
		, inter = wrapped $ \ [ x ] -> O.complement x
		}

line =  Op { name = "line" , arity = 1
		, precedence = Nothing , assoc = AssocNone
		, inter = wrapped $ \ [ x ] -> L.line_graph x
		}

binary :: [ Op Graph_Or_Natural ]
binary = [ times, cross, plus ]

times =  Op { name = "*" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = wrapped $ \ [x, y] -> O.times x y
	      }
cross =  Op { name = "%" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = wrapped $ \ [x, y] -> O.grid x y
	      }
plus = Op { name = "+" , arity = 2
	      , precedence = Just 7 , assoc = AssocLeft
	      , inter = wrapped $ \ [x, y] -> O.union x y
	      }

wrapped fun = Gra . O.normalize . fun . map ( \ ( Gra x ) -> x ) 

