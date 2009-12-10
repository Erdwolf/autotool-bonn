{-# LANGUAGE TemplateHaskell #-}

module Baum.ZweiDrei.Type where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Key a = This a | Infinity 
     deriving ( Typeable, Eq, Ord )

instance Functor Key where
    fmap f (This x ) = This ( f x )
    fmap f Infinity = Infinity

$(derives [makeReader, makeToDoc] [''Key])

-- |  zweites element ist jeweils der schlüssel,
-- schlüssel sind aufsteigend geordnet (je Knoten)
-- in baum stehen jeweils alle knoten,
-- die echt kleiner als schlüssel sind.
-- der letzte Eintrag jeder liste hat key == Infinity
data Baum a = Null 
	    | Baum [ ( Baum a, Key a ) ]
     deriving ( Eq, Ord, Typeable )

instance Functor Baum where
    fmap f Null = Null
    fmap f ( Baum bks ) = Baum $ do
        ( b, k ) <- bks
        return ( fmap f b, fmap f k )

isNull :: Baum a -> Bool
isNull Null = True
isNull _ = False

$(derives [makeReader, makeToDoc] [''Baum])

-- local variables:
-- mode: haskell
-- end;




