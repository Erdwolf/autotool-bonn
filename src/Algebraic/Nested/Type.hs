{-# LANGUAGE DeriveDataTypeable #-}
module Algebraic.Nested.Type 


where

import Autolib.ToDoc hiding ( empty )
import Autolib.Reader

import qualified Autolib.Set as S

import Autolib.Size
import Autolib.Depth

import Data.Typeable

example :: Type Integer
example = read "{ 2, {}, {3, {4}}}"


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

full_size ( Make xs ) = succ $ sum $ map full_item_size $ S.setToList xs

top_length ( Make xs  ) = S.cardinality xs

instance Depth ( Type a ) where
    depth ( Make xs ) = 1 + maximum ( 0 : map depth ( S.setToList xs ) )

flatten ( Make xs ) = concat $ map flatten_item $ S.setToList xs

-----------------------------------------------------------------------

data Item a = Unit a | Packed ( Type a )
    deriving ( Eq, Ord, Typeable )

instance ( Ord a, ToDoc a ) => ToDoc ( Item a ) where
    toDoc ( Unit a ) = toDoc a
    toDoc ( Packed p ) = toDoc p

instance ( Ord a, Reader a ) => Reader ( Item a ) where
    reader 
        =   do n <- reader ; return $ Packed n
        <|> do i <- reader ; return $ Unit   i

instance Size ( Item a ) where
    size ( Unit a ) = 1
    size ( Packed t ) = 1 + size t

full_item_size i = case i of
    Unit a -> 1
    Packed t -> 1 + full_size t

flatten_item i = case i of
    Unit a -> [ a ]
    Packed t -> flatten t

instance Depth ( Item a ) where
    depth ( Unit a ) = 0
    depth ( Packed t ) = depth t


