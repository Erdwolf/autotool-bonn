{-# language DeriveDataTypeable #-}

module Program.List.Expression where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

import Autolib.TES.Identifier
import Data.Char ( isAlphaNum )
import Data.Typeable

data Expression = Scalar Integer
                | Reference Identifier
                | Methodcall Expression Identifier [ Expression ]
    deriving Typeable

instance Size Expression where
    size x = case x of
        Scalar {} -> 1
        Reference {} -> 1
        Methodcall x i xs -> sum $ map size $ x : xs 

example :: Expression
example = read "x.add(3,y.get(2))"

instance ToDoc Expression where
    toDoc x = case x of
        Scalar s -> toDoc s
        Reference i -> toDoc i
        Methodcall self m args -> 
            hcat [ toDoc self, text ".", toDoc m, dutch_tuple $ map toDoc args ]

instance Reader Expression where
    reader = do i <- reader ; return $ Scalar i
         <|> do r <- bezeichner ; ms <- many method 
                return $ foldl apply ( Reference r ) ms

bezeichner = do
    cs <- many1 $ satisfy isAlphaNum
    my_whiteSpace
    return $ mkunary cs
        
method = do
    my_symbol "."
    m <- bezeichner
    args <- my_parens $ my_commaSep reader
    return ( m, args )

apply x ( m, args ) = Methodcall x m args

