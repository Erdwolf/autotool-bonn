module Boolean.Op where

--  $Id$

import Reader 
import ToDoc
import Hash


import qualified TES.Symbol
import qualified TES.Parsec


import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Data.List (partition)

data Op = Op { name  :: String
	     , arity :: Int
	     , precedence :: Maybe Int
	     , assoc :: Assoc
	     , inter :: [Bool] -> Bool
	     }

instance Eq Op where o == p = name o == name p
instance Ord Op where compare o p = compare (name o) (name p)
instance Hash Op where hash = hash . name

instance TES.Symbol.Symbol Op where
    arity = arity
    set_arity a x = x { arity = a }
    precedence = precedence
    assoc = assoc

instance ToDoc Op where 
    toDoc = text . name

instance Show Op where
    show = render . toDoc

instance Reader Op where
    readerPrec p = choice $
       let ( nulls, sonst ) =  partition ( \ op -> 0 == arity op ) ops
           table = makeTokenParser $ emptyDef
		   { commentLine = "" 
		   , commentStart = ""
		   , commentEnd = ""
		   , reservedNames = map name nulls
		   , reservedOpNames = map name sonst
		   }
       in  do op <- sonst
	      return $ do TES.Parsec.reservedOp table ( name op )
			  return op
	++ do op <- nulls
	      return $ do TES.Parsec.reserved table ( name op )
			  return op

instance Read Op where
    readsPrec = parsec_readsPrec

ops :: [ Op ]
ops = nullary ++ unary ++ binary

nullary :: [ Op ]
nullary = do
    (v, cs) <- [ (False, "false"), (True, "true") ]
    return $ Op { name = cs, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = const v
		}

unary :: [ Op ]
unary = do
    return $ Op { name = "!" , arity = 1
		, precedence = Just 10 , assoc = AssocNone
		, inter = \ [x] -> not x
		}

binary :: [ Op ]
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

