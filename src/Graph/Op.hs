{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

module Graph.Op 

( module Expression.Op
, eval, eval0, Graph
, example
) 

where

import Expression.Op
import qualified Autolib.TES.Binu as U

import Autolib.Graph.Graph ( Graph )
import qualified Autolib.Graph.Basic as B
import qualified Autolib.Graph.Ops as O
import qualified Autolib.Graph.Line as L

import Autolib.Hash
import Autolib.ToDoc
import Autolib.Set ( mkSet )
import Autolib.FiniteMap
import Autolib.Reporter

eval0 = eval emptyFM
eval b = 
    let look x = case lookupFM b x of
	    Just y -> return y
	    Nothing -> reject $ text "Graph.Op.eval:" <+> toDoc x
    in  tfoldR look inter

example :: Exp ( Graph Int )
example = read "co P5"

instance Ops ( Graph Int ) where
    bops = U.Binu { U.nullary = nullary
		  , U.unary = unary
		  , U.binary = binary
		  }

nullary :: [ Op ( Graph Int ) ]
nullary = do
    ( tag, fun, start ) <- [ ( "K", B.clique . mkSet, 1 )
		    -- , ( "I", B.independent . mkSet, 2 )
                    , ( "P", B.path , 3  )
                    , ( "C", B.circle , 3)
                    ]
    n <- [ start .. 5 :: Int ]
    return $ Op { name = tag ++ show n
                , arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = wrapped $ \ [ ] -> fun [ 1 .. n ]
		}

unary :: [ Op ( Graph Int ) ]
unary = [ co, line ] 
    
co =  Op { name = "co" , arity = 1
		, precedence = Just 9 , assoc = AssocNone
		, inter = wrapped $ \ [ x ] -> O.complement x
		}

line =  Op { name = "line" , arity = 1
		, precedence = Nothing , assoc = AssocNone
		, inter = wrapped $ \ [ x ] -> L.line_graph x
		}

binary :: [ Op ( Graph Int ) ]
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

wrapped fun = \ xs -> return $ O.normalize $ fun xs

