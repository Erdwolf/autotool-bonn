module Boolean.Op where

--  $Id$

import Reader 
import ToDoc

import qualified TES.Symbol
import qualified TES.Parsec

data Op = Op { name  :: String
	     , arity :: Int
	     , precedence :: Maybe Int
	     , inter :: [Bool] -> Bool
	     }

instance Eq Op where o == p = name o == name p
instance Ord Op where compare o p = compare (name o) (name p)

instance TES.Symbol.Symbol Op where
    arity = arity
    set_arity a x = x { arity = a }
    precedence = precedence

instance ToDoc Op where 
    toDoc = text . name

instance Show Op where
    show = render . toDoc

instance Reader Op where
    readerPrec p = choice $ do
       op <- ops
       return $ do TES.Parsec.symbol TES.Parsec.trs ( name op ) ; return op

instance Read Op where
    readsPrec = parsec_readsPrec

ops :: [ Op ]
ops = nullary ++ unary ++ binary

nullary :: [ Op ]
nullary = do
    (v, cs) <- [ (False, "false"), (True, "true") ]
    return $ Op { name = cs, arity = 0, precedence = Nothing 
		, inter = const v
		}

unary :: [ Op ]
unary = do
    return $ Op { name = "!" , arity = 1, precedence = Just 10 
		, inter = \ [x] -> not x
		}

binary :: [ Op ]
binary = [ Op { name = "&&" , arity = 2, precedence = Just 8 
	      , inter = \ [x, y] -> x && y
	      } ]
      ++ [ Op { name = "||" , arity = 2, precedence = Just 7 
	      , inter = \ [x, y] -> x || y
	      } ]
      ++  do (f, cs) <- [ ((<), "<"), ((<=), "<=")
			, ((==), "=="), ((/=), "!=")
			, ((>=), ">="), ((>), ">") 
			]
	     return $ Op { name = cs, arity = 2, precedence = Just 6 
			 , inter = \ [x,y] -> f x y
			 }

