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

-----------------------------------------------------

data Expression = And Expression Expression
                | Or Expression Expression
                | Not Expression 
                | Var Identifier
    deriving ( Eq, Ord, Typeable )  

instance ToDoc Expression where
    toDocPrec p ( Var i ) = toDoc i
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
        ( do i <- my_identifier ; return $ Var $ mkunary i )


instance Conditions Expression where
    conditions x = case x of
        Var i -> S.fromList [i]
        Not y -> conditions y
        And l r -> S.unions [ conditions l, conditions r ]
        Or  l r -> S.unions [ conditions l, conditions r ]

evaluate :: State -> Expression -> Bool
evaluate s x = case x of 
    Var id -> case M.lookup id s of
             Just v -> v
    Not y -> not $ evaluate s y
    And l r -> and $ map (evaluate s) [ l,r ]
    Or l r -> or $ map (evaluate s) [ l,r ]
