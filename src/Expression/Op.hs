module Expression.Op 

( module Expression.Op
, module Text.ParserCombinators.Parsec.Expr
)

where

--  $Id$

import Autolib.Reader 
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Util.Size

import qualified Autolib.Symbol
import qualified Autolib.TES.Parsec


import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Text.XML.HaXml.Haskell2Xml

import Data.List (partition)

import Data.Typeable

data Op a = Op { name  :: String
	     , arity :: Int
	     , precedence :: Maybe Int
	     , assoc :: Assoc
	     , inter :: [a] -> a
	     }
    deriving ( Typeable )

-- | for parsing
class Ops a where
      ops :: [ Op a ]

-- | xml-darstellung ist einfach ein string (show/read)
instance Ops a => Haskell2Xml (Op a) where
    toContents   = toContents . show
    fromContents cs = 
        let ( x, rest ) = fromContents cs
	in  ( read x, rest )
    toHType _ = Prim "Op" "op" -- ??


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

instance Ops a => Show (Op a) where
    show = render . toDoc

instance Ops a => Reader (Op a) where
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
	      return $ do Autolib.TES.Parsec.reservedOp table ( name op )
			  return op
	++ do op <- nulls
	      return $ do Autolib.TES.Parsec.reserved table ( name op )
			  return op

instance Ops a => Read (Op a) where
    readsPrec = parsec_readsPrec

