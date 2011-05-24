{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable #-}

module Prolog.Query.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

-- | use self-delimiting read/show
-- but nicely formatted reader/todoc
data Query = Query String deriving ( Eq, Ord, Typeable, Read, Show )
data Config = Config [String] String deriving ( Eq, Ord, Typeable, Read, Show )

instance Reader Query where
    reader = do { cs <- getInput; setInput ""; return (Query cs) }
instance Reader Config where
    reader = do cs <- getInput
                setInput ""
                let (x:xs) = lines cs
                return $ Config (words x) (unlines xs)

instance ToDoc Query where
    toDoc (Query cs) = vcat $ map text $ lines cs
instance ToDoc Config where
    toDoc (Config c cs) = vcat $ map text $ (unwords c:) $ lines cs

-- FIXME: should use the size of the syntax tree
instance Size Query where
    size (Query cs) = length cs
instance Size Config where
    size (Config _ cs) = length cs
