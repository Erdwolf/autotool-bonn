{-# OPTIONS -fglasgow-exts #-}

module FP.Env where

import FP.Type

import Autolib.TES.Identifier

import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.Reader

import Text.XML.HaXml.Haskell2Xml
import Data.Typeable


data Env = Env ( FiniteMap Identifier Type )
    deriving Typeable

instance ToDoc Env where
   toDoc ( Env env ) = dutch Nothing ( text "{" , semi, text "}" ) $ do
       ( n, t ) <- fmToList env
       return $ toDoc n <+> text "::" <+> toDoc t

instance Reader Env where
   reader = my_braces $ do
       nts <- do
           n <- reader
           my_reserved "::"
           t <- reader
           return (n, t )
         `Autolib.Reader.sepBy` my_reserved ";"
       return $ Env $ listToFM nts
