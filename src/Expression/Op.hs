module Expression.Op 

( module Expression.Op
, module Text.ParserCombinators.Parsec.Expr
, Identifier, vars, syms, draw, T.tfold
)

where

--  $Id$

import Autolib.Reader 
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Size

import qualified Autolib.Symbol
import qualified Autolib.TES.Parsec
import qualified Autolib.TES.In as I
import qualified Autolib.TES.Term as T
import Autolib.TES.Position ( vars, syms )
import Autolib.TES.Draw ( draw )

import Autolib.TES.Identifier
import Autolib.Xml

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Text.XML.HaXml.Haskell2Xml

import Data.List (partition)

import Data.Typeable

type Exp a = T.Term Identifier ( Op a )

data Op a = Op { name  :: String
	     , arity :: Int
	     , precedence :: Maybe Int
	     , assoc :: Assoc
	     , inter :: [a] -> a
             , prefix :: Bool
	     }
    deriving ( Typeable )

-- | for parsing
class Ops a where
      ops :: [ Op a ]
      funs :: [ Op a ]

-- | xml-darstellung ist einfach ein string (show\/read)
instance Ops a => Container (Op a) String where
    label _ = "Op"
    pack = show
    unpack = read

instance Eq (Op a) where o == p = name o == name p
instance Ord (Op a) where compare o p = compare (name o) (name p)
instance Hash (Op a) where hash = hash . name
instance Size (Op a) where size = const 1

instance Ops a => Autolib.Symbol.Symbol (Op a) where
    arity = arity
    set_arity a x = x { arity = a }
    precedence = precedence
    assoc = assoc

instance Ops a => ToDoc (Op a) where 
    toDoc = text . name

instance Ops a => Reader (Op a) where
    readerPrec p = choice $
       let ( nulls, sonst ) =  
               partition ( \ op -> 0 == arity op ) ops
               -- FIXME: handling of funs
           table = makeTokenParser $ emptyDef
		   { commentLine = "" 
		   , commentStart = ""
		   , commentEnd = ""
		   , reservedNames = map name nulls
		   , reservedOpNames = map name sonst
		   }
       in  do op <- sonst
	      return $ do Autolib.TES.Parsec.reservedOp table ( name op )
			  return op
	++ do op <- nulls
	      return $ do Autolib.TES.Parsec.reserved table ( name op )
			  return op

instance Ops a => Reader ( Exp a ) where
    readerPrec p = I.treader 
                 $ I.Config { I.reserved_operator_symbols = ops
                            , I.allow_new_symbols = False
                            }

