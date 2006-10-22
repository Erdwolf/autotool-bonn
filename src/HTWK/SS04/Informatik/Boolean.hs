module HTWK.SS04.Informatik.Boolean where

--  $Id$

import Boolean.Instance hiding ( make ) 
import qualified Boolean.Quiz

import Inter.Types
import Inter.Make

import Data.Dynamic

make :: String -> BI -> IO Variant
make t bi = return $ Variant $ ( inner_make bi ) { tag = "Boolean-" ++ t }

inner_make bi =
       Var { problem = Boolean
	   , tag = "Boolean"
	   , key = \ matrikel -> return matrikel
	   , gen = \ key -> do
	         return $ return bi
	   }

makers :: [ Make ]
makers =  [ Make "Boolesche Ausdrücke"
	         inner_make 
                 (configs0 !! 0) -- example
	  ]

configs :: [ Dynamic ]
configs = map toDyn configs0

configs0 =
    [ BI { formula = read "x == (y == z)"
      	, operators = read "mkSet [ false, true, !, ||, && ]"
      	}
    , BI { formula = read "x || y && !z"
      	, operators = read "mkSet [ false, true, <= ]"
      	}
    , BI { formula = read "(p == q) && (q != r) || (p != s)"
      	, operators = read "mkSet [ false, true, !, ||, && ]"
      	}
    ]

generates :: [ IO Variant ]
generates = 
    [ make "A" $ BI { formula = read "x == (y == z)"
		, operators = read "mkSet [ false, true, !, ||, && ]"
		}
    , make "B" $ BI { formula = read "x || y && !z"
		, operators = read "mkSet [ false, true, <= ]"
		}
    , make "C" $ BI { formula = read "(p == q) && (q != r) || (p != s)"
		, operators = read "mkSet [ false, true, !, ||, && ]"
		}
    , Boolean.Quiz.qmake 5
    ]

     