module HTWK.SS04.Informatik.Boolean where

--  $Id$

import Boolean.Instance

import Inter.Types

make :: BI -> IO Variant
make bi = return $ Variant 
     $ Var { problem = Boolean
	   , aufgabe = "Boolean"
	   , version = tag bi
	   , key = \ matrikel -> return matrikel
	   , gen = \ key -> do
	         return $ return bi
	   }

generates :: [ IO Variant ]
generates = 
    [ make $ BI { tag = "A"
		, formula = read "x == (y == z)"
		, operators = read "mkSet [ false, true, !, ||, && ]"
		}
    , make $ BI { tag = "B"
		, formula = read "x || y && !z"
		, operators = read "mkSet [ false, true, <= ]"
		}
    , make $ BI { tag = "C"
		, formula = read "(p == q) && (q != r) || (p != s)"
		, operators = read "mkSet [ false, true, !, ||, && ]"
		}

    ]

     