{-# OPTIONS -fglasgow-exts #-}

module Algebraic.Nested 

( Type , unit, empty
, example
, union, difference, intersection, power
)

where

import Autolib.ToDoc hiding ( empty )
import Autolib.Reader

import qualified Autolib.Set as S

import Autolib.Size

import Data.Typeable

example :: Type Integer
example = read "{ 2, {}, {3, {4}}}"

unit :: Ord a => a -> Type a 
unit x = Make $ S.mkSet [ Unit x ]

empty :: Type a
empty = Make $ S.emptySet

union :: Ord a => Type a -> Type a -> Type a 
union ( Make xs ) ( Make ys ) = Make $ S.union xs ys

difference :: Ord a => Type a -> Type a -> Type a
difference ( Make xs ) ( Make ys ) = Make $ S.minusSet xs ys

intersection :: Ord a => Type a -> Type a -> Type a
intersection ( Make xs ) ( Make ys ) = Make $ S.intersect xs ys

power :: Ord a => Type a -> Type a
power ( Make xs ) = Make $ S.mkSet $ map ( Packed. Make ) $ S.subsets xs

data Type a = Make ( S.Set ( Item a ))
    deriving ( Eq, Ord, Typeable )

instance ( Ord a, ToDoc a ) => ToDoc ( Type a ) where
    toDoc ( Make xs ) 
        = braces $ fsep $ punctuate comma 
        $ map toDoc $ S.setToList xs

instance ( Ord a, Reader a ) => Reader ( Type a ) where
    reader = my_braces $ do
        xs <- Autolib.Reader.sepBy reader my_comma 
        return $ Make $ S.mkSet xs

instance Size ( Type a ) where
    size ( Make xs ) = sum $ map size $ S.setToList xs

data Item a = Unit a | Packed ( Type a )
    deriving ( Eq, Ord, Typeable )

instance Size ( Item a ) where
    size ( Unit a ) = 1
    size ( Packed t ) = 1 + size t

instance ( Ord a, ToDoc a ) => ToDoc ( Item a ) where
    toDoc ( Unit a ) = toDoc a
    toDoc ( Packed p ) = toDoc p

instance ( Ord a, Reader a ) => Reader ( Item a ) where
    reader 
        =   do n <- reader ; return $ Packed n
        <|> do i <- reader ; return $ Unit   i

