{-# language DeriveDataTypeable #-}

module Peano.Data where

import qualified Peano.Type

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Size

import Data.Typeable


data Exp = Const Integer
         | Ref String
         | Abs String Peano.Type.Type Exp
         | App Exp Exp
         | Fold Exp Exp
    deriving ( Eq, Ord, Typeable )

instance Size Exp where
    size x = case x of
        Const _ -> 1
        Ref _ -> 1
        Abs n t x -> succ $ size x
        App f a -> succ $ size f + size a
        Fold z s -> succ $ size z + size s

example :: Exp
example = read "fold ( \\ ( y :: N ) -> y ) ( \\ (f :: N -> N) -> \\ (y :: N) -> s (f y)) "

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
    <|> do 
        my_reserved "fold"
        z <- atomic
        s <- atomic
        return $ Fold z s
    <|> do i <- my_identifier ; return $ Ref i

instance ToDoc Exp where
    toDocPrec p x = case x of
        Const i -> toDoc i
        Ref s -> text s
        Abs n t x -> parenthesized p $ hsep 
            [ text "\\" 
            , parens $ hsep [ text n, text "::", toDoc t ]
            , text "->"
            , toDoc x
            ]
        App f a -> parenthesized p 
            $ hsep [ toDoc f , toDocPrec 9 a ]
        Fold z s -> parenthesized p 
            $ hsep [ text "fold", toDocPrec 9 z, toDocPrec 9 s ]

parenthesized p =
    if p > 0 then parens else id