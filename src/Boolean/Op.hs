module Boolean.Op 

( module Expression.Op
) 

where

--  $Id$

import Expression.Op

instance Ops Bool where
    ops = nullary ++ unary ++ binary

nullary :: [ Op Bool ]
nullary = do
    (v, cs) <- [ (False, "false"), (True, "true") ]
    return $ Op { name = cs, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = const v
		}

unary :: [ Op Bool ]
unary = do
    return $ Op { name = "!" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = \ [x] -> not x
		}

binary :: [ Op Bool ]
binary = [ Op { name = "&&" , arity = 2
	      , precedence = Just 8 , assoc = AssocLeft
	      , inter = \ [x, y] -> x && y
	      } ]
      ++ [ Op { name = "||" , arity = 2
	      , precedence = Just 7 , assoc = AssocLeft
	      , inter = \ [x, y] -> x || y
	      } ]
      ++  do (f, cs) <- [ ((<), "<"), ((<=), "<=")
			, ((==), "=="), ((/=), "!=")
			, ((>=), ">="), ((>), ">") 
			]
	     return $ Op { name = cs, arity = 2
			 , precedence = Just 6 , assoc = AssocNone
			 , inter = \ [x,y] -> f x y
			 }

