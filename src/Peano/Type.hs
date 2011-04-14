{-# LANGUAGE DeriveDataTypeable #-}

module Peano.Type where

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

-- import qualified Text.Parsec as TP
import qualified Text.ParserCombinators.Parsec as TP

import Data.List ( intersperse )

data Type = Nat | App Type Type
    deriving ( Eq, Ord, Typeable )

spine :: Type -> [ Type ]
spine t = case t of
    App l r -> l : spine r
    Nat -> []

unspine :: [ Type ] -> Type -> Type
unspine ts t = foldr App t ts

instance ToDoc Type where
    toDocPrec p t = case spine t of
            [] -> text "N"
            ts -> ( if p > 0 then parens else id )
               $ hsep 
               $ intersperse ( text "->" )
               $ map ( toDocPrec 9 ) $ ts ++ [ Nat ]
                
instance Reader Type where
    reader = do 
        ts <- TP.sepBy atomic $ my_reserved "->"
        return $ unspine ( init ts ) ( last ts )
        
atomic = do my_reserved "N" ; return Nat
     <|> my_parens reader     

