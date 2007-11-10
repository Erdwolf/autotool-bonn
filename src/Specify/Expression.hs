{-# language GADTs #-}
{-# OPTIONS -fglasgow-exts -XFlexibleInstances -fno-monomorphism-restriction #-}

module Specify.Expression where

import Autolib.Set
import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

data Expression a where

    Constant :: ToDoc a => a -> Expression a        
--    Variable :: Identifier -> Expression Integer
    Apply :: Identifier -> [ Expression Integer ] -> Expression Integer

    Plus :: Expression Integer -> Expression Integer -> Expression Integer
    Minus :: Expression Integer -> Expression Integer -> Expression Integer
    Negate :: Expression Integer -> Expression Integer 
    Times :: Expression Integer -> Expression Integer -> Expression Integer
    Quotient :: Expression Integer -> Expression Integer -> Expression Integer
    Remainder :: Expression Integer -> Expression Integer -> Expression Integer

--    Forall :: Set Identifier -> Expression Bool -> Expression Bool

    Less :: Expression Integer -> Expression Integer -> Expression Bool
    LessEqual :: Expression Integer -> Expression Integer -> Expression Bool
    Equal :: ( Eq a, ToDoc a ) => Expression a -> Expression a -> Expression Bool
    GreaterEqual :: Expression Integer -> Expression Integer -> Expression Bool
    Greater :: Expression Integer -> Expression Integer -> Expression Bool
    NotEqual :: ( Eq a, ToDoc a ) => Expression a -> Expression a -> Expression Bool

    And :: Expression Bool -> Expression Bool -> Expression Bool
    Or :: Expression Bool -> Expression Bool -> Expression Bool
    Implies :: Expression Bool -> Expression Bool -> Expression Bool
    Not :: Expression Bool -> Expression Bool

    Branch :: Expression Bool -> Expression a -> Expression a -> Expression a

    deriving Typeable

instance ToDoc a => ToDoc ( Expression a ) where
    toDocPrec p x = case x of
        Constant a -> toDoc a
--        Variable v -> toDoc v

	Apply fun args -> toDoc fun <+> case args of
	      [] -> empty
	      _ -> parens ( Autolib.ToDoc.sepBy comma $ map toDoc args )
        
        Plus      x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "+", toDocPrec 6 y ]
        Minus     x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "-", toDocPrec 6 y ]
        Negate    x   -> docParen ( p > 9 ) $ hsep [ text "-", toDocPrec 9 x                ]
        Times     x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "*", toDocPrec 8 y ]
        Quotient  x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "/", toDocPrec 8 y ]
        Remainder x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "%", toDocPrec 8 y ]

--	Forall xs y   -> docParen ( p > 1 ) 
--	   $ hsep [ text "forall", hsep $ map toDoc $ setToList xs, text ".", toDocPrec 1 y ]

	Branch c y z -> vcat [ text "if" <+> toDoc c, text "then" <+> toDoc y, text "else" <+> toDoc z ]

        Less      x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "<", toDocPrec 5 y ]
        LessEqual x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "<=", toDocPrec 5 y ]
        Equal     x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "==", toDocPrec 5 y ]
        GreaterEqual x y ->docParen (p> 5 ) $ hsep [ toDocPrec 5 x, text ">=", toDocPrec 5 y ]
        Greater   x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text ">", toDocPrec 5 y ]
        NotEqual  x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "!=", toDocPrec 5 y ]

        Or        x y -> docParen ( p > 1 ) $ hsep [ toDocPrec 1 x, text "||", toDocPrec 2 y ]
        And       x y -> docParen ( p > 3 ) $ hsep [ toDocPrec 3 x, text "&&", toDocPrec 4 y ]
        Implies   x y -> docParen ( p > 2 ) $ hsep [ toDocPrec 2 x, text "==>", toDocPrec 3 y ]
        Not       x   -> docParen ( p > 4 ) $ hsep [ text "!", toDocPrec 8 x ]

instance Size ( Expression a ) where
    size x = case x of
        Constant a -> 1

	Apply fun args -> 1 + sum ( map size args )
        
        Plus      x y -> 1 + size x + size y
        Minus     x y -> 1 + size x + size y
        Negate    x   -> 1 + size x 
        Times     x y -> 1 + size x + size y
        Quotient  x y -> 1 + size x + size y
        Remainder x y -> 1 + size x + size y

--	Forall xs y   -> docParen ( p > 1 ) 
--	   $ hsep [ text "forall", hsep $ map toDoc $ setToList xs, text ".", toDocPrec 1 y ]

	Branch c y z ->  1 + size c + size y + size z

        Less      x y -> 1 + size x + size y
        LessEqual x y -> 1 + size x + size y
        Equal     x y -> 1 + size x + size y
        GreaterEqual x y ->1 + size x + size y
        Greater   x y -> 1 + size x + size y
        NotEqual  x y -> 1 + size x + size y

        Or        x y -> 1 + size x + size y
        And       x y -> 1 + size x + size y
        Implies   x y -> 1 + size x + size y
        Not       x   -> 1 + size x 

instance Reader ( Expression Bool ) where
    reader = 
        let binop name f assoc   =
                 Infix ( do { my_symbol name; return $ f }  ) assoc
            unop name f =
                 Prefix ( do { my_symbol name; return $ f }  ) 
	in  buildExpressionParser
	        [ [ binop "==>" Implies AssocNone ]
		, [ binop "==" Equal AssocNone 
		  , binop "!=" NotEqual AssocNone
		  ]
		, [ binop "||" Or AssocLeft ]
		, [ binop "&&" And AssocLeft ]
		, [ unop "!" Not ] 
                ] 
                ( my_parens reader 
		<|> do my_reserved "true" ; return $ Constant True
		<|> do my_reserved "false" ; return $ Constant False
		<|> branch
		<|> comparison
		)

comparison = do
    x <- reader
    op <- foldr1 (<|>) $ do 
        ( name, val ) <- [  ("<=", LessEqual ),( "<", Less ) , ("==", Equal )
			 , ( ">=", GreaterEqual), (">", Greater), ("!=", NotEqual) 
			 ]
	return $ do
            my_symbol name
	    return val
    y <- reader
    return $ op x y

instance Reader ( Expression Integer ) where
    reader = 
        let binop name f assoc   =
                 Infix ( do { my_symbol name; return $ f }  ) assoc
            unop name f =
                 Prefix ( do { my_symbol name; return $ f }  ) 
	in  buildExpressionParser
	        [ [ unop "-" Negate ] 
		, [ binop "*" Times AssocLeft
		  , binop "/" Quotient AssocLeft
		  , binop "%" Remainder AssocLeft
		  ]
		, [ binop "+" Plus AssocLeft 
		  , binop "-" Minus AssocLeft 
		  ]
                ] 
                ( my_parens reader 
		<|> do i <- my_integer ; return $ Constant i
		<|> application
		<|> branch
		)


branch = do
    my_reserved "if"
    c <- reader
    my_reserved "then"
    y <- reader
    my_reserved "else"
    z <- reader
    return $ Branch c y z

application = do
    fun <- ident
    args <- option [] $ my_parens $ Autolib.Reader.sepBy reader my_comma 
    return $ Apply fun args

ident = fmap mkunary my_identifier
