{-# OPTIONS -fallow-overlapping-instances #-}

module Boolean.Op 

( module Expression.Op
, und, oder, implies, nicht
) 

where



import Expression.Op
import qualified Autolib.TES.Binu as B

import Autolib.Hash

instance Ops Bool where
    bops = B.Binu
        { B.nullary = nullary 
	, B.unary = unary ++ functions
	, B.binary = binary
	}

nullary :: [ Op Bool ]
nullary = do
    (v, cs) <- [ (False, "false"), (True, "true") ]
    return $ Op { name = cs, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = lift0 $  v
		}

functions :: [ Op Bool ]
functions = do
    ( cs, ar, fun ) <-
        [ ( "add"  , 3, \ xs -> toEnum $ sum (map fromEnum xs) `mod` 2 ) 
        , ( "carry", 3, \ xs -> 1 <= sum (map fromEnum xs) )
        , ( "nand" , 2, \ xs -> not $ and xs )
        , ( "nor"  , 2, \ xs -> not $ or  xs )
        ]
    return $ Op { name = cs, arity = ar
		, precedence = Nothing , assoc = AssocNone
		, inter = lift $ fun
		}

unary :: [ Op Bool ]
unary = [     Op { name = "!" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = lift1 $ not
		}
        , nicht
	]

nicht =     Op { name = "not" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = lift1 $ not
		}

und =  Op { name = "&&" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = lift2 ( && )
	      }
oder = Op { name = "||" , arity = 2
	      , precedence = Just 7 , assoc = AssocLeft
	      , inter = lift2 (||)
	      }

implies = Op { name = "->", arity = 2
			 , precedence = Just 6 , assoc = AssocNone
			 , inter = lift2 (<=)
			 }


binary :: [ Op Bool ]
binary = [ und, oder ]
      ++  do (f, cs) <- [ ((<), "<")
			, ((<=), "<="), ((<=), "->")
			, ((==), "=="), ((==), "<->")
			, ((/=), "!=")
			, ((>=), ">="), ((>), ">") 
                        , ( \ x y -> not (x && y) , "!&" )
                        , ( \ x y -> not (x || y) , "!|" )
			]
	     return $ Op { name = cs, arity = 2
			 , precedence = Just 6 , assoc = AssocNone
			 , inter = lift2 f
			 }

