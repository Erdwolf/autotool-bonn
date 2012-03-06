{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable #-}

module Prolog.Programming.Data where

import Autolib.ToDoc (ToDoc(..), vcat, text, nest)
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Language.Prolog (term, Term)
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*))

data Facts = Facts String deriving ( Eq, Ord, Typeable, Read, Show )
data Config = Config String deriving ( Eq, Ord, Typeable, Read, Show  )

instance Reader Facts where
    reader = do { cs <- getInput; setInput ""; return (Facts cs) }
instance Reader Config where
    reader = do { cs <- getInput; setInput ""; return (Config cs) }

instance ToDoc Facts where
    toDoc (Facts cs) = text cs
instance ToDoc Config where
    toDoc (Config cs) = nest (length "gelesen: ") $ text cs

-- FIXME: should use the size of the syntax tree
instance Size Facts where
    size (Facts cs) = length cs
instance Size Config where
    size (Config cs) = length cs

