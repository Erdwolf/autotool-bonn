{-# OPTIONS -fglasgow-exts #-}

module Flow.Program where

import Flow.Expression
import Flow.Conditions

import Autolib.TES.Identifier

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Data.Set ( Set )
import qualified Data.Set as S

data Program s = Program [ s ]
    deriving ( Eq, Ord, Typeable )

instance ToDoc s => ToDoc ( Program s ) where
    toDoc ( Program stmts ) = vcat $ map toDoc stmts

instance Reader s => Reader ( Program s ) where
    reader = do
        stmts <- many reader
	return $ Program stmts

instance Size s => Size ( Program s ) where
    size ( Program sts ) = sum $ map size sts

instance Conditions s => Conditions ( Program s ) where
    conditions ( Program ss ) = conditions ss

