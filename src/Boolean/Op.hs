module Boolean.Op 

( module Expression.Op
) 

where

--  $Id$

import Expression.Op

instance Ops Bool where
    ops = nullary ++ unary ++ binary ++ functions

nullary :: [ Op Bool ]
nullary = do
    (v, cs) <- [ (False, "false"), (True, "true") ]
    return $ Op { name = cs, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = const v
                , prefix = False -- FIXME
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
		, inter = fun
                , prefix = False -- FIXME
		}

unary :: [ Op Bool ]
unary = do
    return $ Op { name = "!" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = \ [x] -> not x
                , prefix = False -- FIXME
		}

binary :: [ Op Bool ]
binary = [ Op { name = "&&" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = \ [x, y] -> x && y
                , prefix = False -- FIXME
	      } ]
      ++ [ Op { name = "||" , arity = 2
	      , precedence = Just 7 , assoc = AssocLeft
	      , inter = \ [x, y] -> x || y
                , prefix = False -- FIXME
	      } ]
      ++  do (f, cs) <- [ ((<), "<"), ((<=), "<=")
			, ((==), "=="), ((/=), "!=")
			, ((>=), ">="), ((>), ">") 
                        , ( \ x y -> not (x && y) , "!&" )
                        , ( \ x y -> not (x || y) , "!|" )
			]
	     return $ Op { name = cs, arity = 2
			 , precedence = Just 6 , assoc = AssocNone
			 , inter = \ [x,y] -> f x y
                         , prefix = False -- FIXME
			 }

