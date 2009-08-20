{-# OPTIONS -cpp -fth #-}
{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, TemplateHaskell #-}


module Hanoi.Type where

--  $Id$

import Hanoi.Restriction

import Condition

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size 

import Data.Typeable

import Gateway.Help

import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType
import Inter.Types ()


type Scheibe = Integer

data Turm = A | B | C | D
    deriving ( Eq, Ord, Enum, Bounded, Typeable )

instance Source Turm where source _ = drift __FILE__

$(derives [makeReader, makeToDoc] [''Turm])
-- {-! for Turm derive: Reader, ToDoc !-}

type Hof = FiniteMap Turm [ Scheibe ]

instance XmlRpcType Hof where
    getType _ = TStruct
    toValue h = ValueStruct $ do
        ( k, v ) <- fmToList h
        return ( show k, toValue v )
    fromValue ( ValueStruct kvs ) = do
        kvs <- sequence $ do
             ( k, v) <- kvs
             return $ do
                 vv <- fromValue v
                 return ( read k, vv )
        return $ listToFM kvs

type Zug = ( Turm, Turm )

instance Size Zug where size _ = 1

data HI = HI { start :: Hof
	     , ziel  :: Hof
	     , restriction :: Restriction
	     }
    deriving ( Typeable )

$(asXmlRpcStruct ''HI)

instance Source HI where source _ = drift __FILE__

$(derives [makeReader, makeToDoc] [''HI])
-- {-! for HI derive: Reader, ToDoc !-}


-- local variables:
-- mode: haskell
-- end:






