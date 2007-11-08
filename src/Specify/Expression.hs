{-# language GADTs #-}
{-# OPTIONS -fglasgow-exts -XFlexibleInstances #-}

module Specify.Expression where

import Autolib.TES.Identifier
import Autolib.ToDoc
import Autolib.Reader

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr hiding ( Operator )

import Data.Typeable

data Expression a where

    Constant :: ToDoc a => a -> Expression a        
    Variable :: Identifier -> Expression Integer

    Plus :: Expression Integer -> Expression Integer -> Expression Integer
    Minus :: Expression Integer -> Expression Integer -> Expression Integer
    Negate :: Expression Integer -> Expression Integer 
    Times :: Expression Integer -> Expression Integer -> Expression Integer
    Quotient :: Expression Integer -> Expression Integer -> Expression Integer
    Remainder :: Expression Integer -> Expression Integer -> Expression Integer

    Less :: Expression Integer -> Expression Integer -> Expression Bool
    LessEqual :: Expression Integer -> Expression Integer -> Expression Bool
    Equal :: ToDoc a => Expression a -> Expression a -> Expression Bool
    GreaterEqual :: Expression Integer -> Expression Integer -> Expression Bool
    Greater :: Expression Integer -> Expression Integer -> Expression Bool
    NotEqual :: ToDoc a => Expression a -> Expression a -> Expression Bool

    And :: Expression Bool -> Expression Bool -> Expression Bool
    Or :: Expression Bool -> Expression Bool -> Expression Bool
    Implies :: Expression Bool -> Expression Bool -> Expression Bool
    Not :: Expression Bool -> Expression Bool

    deriving Typeable

instance ToDoc a => ToDoc ( Expression a ) where
    toDocPrec p x = case x of
        Constant a -> toDoc a
        Variable v -> toDoc v
        
        Plus      x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "+", toDocPrec 6 y ]
        Minus     x y -> docParen ( p > 5 ) $ hsep [ toDocPrec 5 x, text "-", toDocPrec 6 y ]
        Negate    x   -> docParen ( p > 9 ) $ hsep [ text "-", toDocPrec 9 x                ]
        Times     x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "*", toDocPrec 8 y ]
        Quotient  x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "/", toDocPrec 8 y ]
        Remainder x y -> docParen ( p > 7 ) $ hsep [ toDocPrec 7 x, text "%", toDocPrec 8 y ]

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
		<|> comparison
		<|> do my_reserved "true" ; return $ Constant True
		<|> do my_reserved "false" ; return $ Constant False
		)

comparison = do
    x <- reader
    op <- foldr1 (<|>) $ do 
        ( name, val ) <- [ ( "<", Less ) , ("<=", LessEqual ), ("==", Equal )
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
	        [ [ binop "+" Plus AssocLeft 
		  , binop "-" Minus AssocLeft 
		  ]
		, [ binop "*" Times AssocLeft
		  , binop "/" Quotient AssocLeft
		  , binop "%" Remainder AssocLeft
		  ]
		, [ unop "-" Negate ] 
                ] 
                ( my_parens reader 
		<|> do i <- my_integer ; return $ Constant i
		<|> do i <- my_identifier ; return $ Variable $ mkunary i
		)


