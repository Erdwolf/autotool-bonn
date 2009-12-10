module Program.Array.Expression where

import Program.Array.Operator

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Autolib.Util.Zufall

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

-- | access to array element
data Access = Access Identifier [ Expression ]
    deriving Typeable

instance Size Access where
    size ( Access name inds ) = 1 + sum ( map size inds )

instance ToDoc Access where
    toDoc ( Access name inds ) = 
        toDoc name <+> hsep ( do ind <- inds ; return $ brackets $ toDoc ind )

instance Reader Access where
    reader = do
        name <- reader
	inds <- many $ my_brackets $ reader
	return $ Access name inds

-- | arithmetical expression, with multi-dimensional array access
data Expression = Reference Access
	| Literal Integer
	| Binary Operator Expression Expression
    deriving Typeable

instance Size Expression where
     size exp = case exp of
          Reference acc -> size acc
	  Literal i -> 1
	  Binary op l r -> 1 + size l + size r


instance ToDoc Expression where
    toDocPrec p e = case e of
        Reference acc -> toDoc acc
	Literal i -> toDoc i
        Binary op l r -> 
	    case op of
                Add      -> docParen ( p > 1 ) 
			 $ hsep [ toDocPrec 1 l , text "+" , toDocPrec 2 r ]
                Subtract -> docParen ( p > 3 ) 
			 $  hsep [ toDocPrec 3 l , text "-" , toDocPrec 4 r ]
                Multiply -> docParen ( p > 5 ) 
			 $  hsep [ toDocPrec 5 l , text "*" , toDocPrec 6 r ]
                Divide   -> docParen ( p > 7 ) 
			 $  hsep [ toDocPrec 7 l , text "/" , toDocPrec 8 r ]

instance Reader Expression where
    reader = buildExpressionParser operators atomic

operators = 
        [ [ op "*" Multiply AssocLeft
          , op "/" Divide AssocLeft
          ]
        , [ op "+" Add AssocLeft
          , op "-" Subtract AssocLeft
          ]
        ]
    where
      op name f assoc   =
         Infix ( do { my_symbol name; return $ Binary f }  ) assoc

atomic :: Parser Expression
atomic = my_parens reader
     <|> do i <- my_integer ; return $ Literal i
     <|> do a <- reader     ; return $ Reference a

