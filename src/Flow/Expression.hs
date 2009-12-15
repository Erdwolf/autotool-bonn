{-# OPTIONS -fglasgow-exts #-}

module Flow.Expression where

import Flow.Conditions
import Flow.State

import Autolib.TES.Identifier
import Data.Set ( Set )
import qualified Data.Set as S
import Data.Map ( Map )
import qualified Data.Map as M

import Autolib.ToDoc
import Autolib.Reader
import Text.ParserCombinators.Parsec.Expr

import Data.Typeable
import Data.Char ( toLower )

-----------------------------------------------------

data Expression = And Expression Expression
                | Or Expression Expression
                | Not Expression 
                | Var Identifier
                | Constant Bool
    deriving ( Eq, Ord, Typeable )  

instance ToDoc Expression where
    toDocPrec p ( Var i ) = toDoc i
    toDocPrec p ( Constant b ) = 
        text $ map toLower $ show b
    toDocPrec p ( Not x ) = 
        text "!" <+> toDocPrec 8 x
    toDocPrec p ( And x y ) 
        = ( if p > 6 then parens else id )
        $ toDocPrec 6 x <+> text "&&" <+> toDocPrec 7 y
    toDocPrec p ( Or x y ) 
        = ( if p > 4 then parens else id )
        $ toDocPrec 4 x <+> text "||" <+> toDocPrec 5 y

instance Reader Expression where
    reader = buildExpressionParser
        [ [ Prefix ( do my_symbol "!" ; return Not ) ]
        , [ Infix  ( do my_symbol "&&" ; return And ) AssocLeft ]
        , [ Infix  ( do my_symbol "||" ; return Or ) AssocLeft ]
        ]
        atom

atom = my_parens reader 
    <|> do my_reserved "true" ; return $ Constant True
    <|> do my_reserved "false" ; return $ Constant False
    <|> do i <- my_identifier ; return $ Var $ mkunary i 



instance Conditions Expression where
    conditions x = case x of
        Constant {} -> S.empty
        Var i -> S.fromList [i]
        Not y -> conditions y
        And l r -> S.unions [ conditions l, conditions r ]
        Or  l r -> S.unions [ conditions l, conditions r ]

evaluate :: State -> Expression -> Bool
evaluate s x = case x of 
    Constant b -> b
    Var id -> case Flow.State.lookup id s of
             Just v -> v
    Not y -> not $ evaluate s y
    And l r -> and $ map (evaluate s) [ l,r ]
    Or l r -> or $ map (evaluate s) [ l,r ]
