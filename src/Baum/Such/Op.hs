{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, DeriveDataTypeable #-}

module Baum.Such.Op where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import Autolib.Reporter
import System.Random

class    ( Eq a, Ord a, Show a, ToDoc a, Reader a, Random a ) 
    => OpC a
instance ( Eq a, Ord a, Show a, ToDoc a, Reader a, Random a ) 
    => OpC a

data OpC a => Op a = Insert a | Delete a | Any
     deriving ( Eq, Typeable )

$(derives [makeReader] [''Op])

instance OpC a => ToDoc (Op a) where
    toDoc (Insert a) = text "Insert" <+> toDoc a
    toDoc (Delete a) = text "Delete" <+> toDoc a
    toDoc Any        = text "Any"

newtype OpList a = OpList [Op a] deriving (Typeable)

instance OpC a => Reader (OpList a) where
    reader = do
        ops <- reader
        return (OpList ops)

instance OpC a => ToDoc (OpList a) where
    toDoc (OpList ops) = text (show ops)

