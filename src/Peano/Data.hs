module Peano.Data where

import qualified Peano.Type

import Autolib.ToDoc
import Autolib.Reader

data Exp = Ref String
         | Abs String Peano.Type.Type Exp
         | App Exp Exp
    deriving ( Eq )

instance Reader Exp where
    reader = do 
        x : xs <- many1 atomic
        return $ foldl App x xs

atomic = my_parens reader
    <|> do 
        my_reserved "\\"
        ( n, t ) <- my_parens $ do
            n <- my_identifier
            my_reserved "::"
            t <- reader
            return ( n, t )
        my_reserved "->"
        x <- reader
        return $ Abs n t x
    <|> do i <- my_identifier ; return $ Ref i

instance ToDoc Exp where
    toDocPrec p x = case x of
        Ref s -> text s
        Abs n t x -> parenthesized p $ hsep 
            [ text "\\" 
            , parens $ hsep [ text n, text "::", toDoc t ]
            , text "->"
            , toDoc x
            ]
        App f a -> parenthesized p 
            $ hsep [ toDoc f , toDocPrec 9 a ]

parenthesized p =
    if p > 0 then parens else id