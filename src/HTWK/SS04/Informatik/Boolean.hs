module HTWK.SS04.Informatik.Boolean where

--  $Id$

import Boolean.Instance
import qualified Boolean.Quiz

import Inter.Types
import Inter.Make

import Data.Dynamic

make :: BI -> IO Variant
make bi = return $ Variant $ inner_make bi

inner_make bi =
       Var { problem = Boolean
	   , aufgabe = "Boolean"
	   , version = tag bi
	   , key = \ matrikel -> return matrikel
	   , gen = \ key -> do
	         return $ return bi
	   }

makers :: [ Make ]
makers =  [ Make inner_make ]

configs :: [ Dynamic ]
configs = map toDyn
    [ BI { tag = "A"
      	, formula = read "x == (y == z)"
      	, operators = read "mkSet [ false, true, !, ||, && ]"
      	}
    , BI { tag = "B"
      	, formula = read "x || y && !z"
      	, operators = read "mkSet [ false, true, <= ]"
      	}
    , BI { tag = "C"
      	, formula = read "(p == q) && (q != r) || (p != s)"
      	, operators = read "mkSet [ false, true, !, ||, && ]"
      	}
    ]

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
    , Boolean.Quiz.make 5
    ]

     