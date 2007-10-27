module Program.Array.Statement where

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

data Statement = Assign Access Expression

data Expression = Reference Access
	| Literal Integer
	| Binary Op Expression Expression

data Op = Add | Subtract | Multiply | Divide

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
atomic = do i <- my_integer ; return $ Literal i
     <|> do a <- reader     ; return $ Reference a

-- | access to array element
data Access = Access Identifier [ Expression ]

instance ToDoc Access where
    toDoc ( Access name inds ) = 
        toDoc name <+> hsep ( do ind <- inds ; return $ brackets $ toDoc ind )

instance Reader Access where
    reader = do
        name <- reader
	inds <- many $ my_brackets $ reader
	return $ Access name inds


