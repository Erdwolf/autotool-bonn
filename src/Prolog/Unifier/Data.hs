{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable #-}

module Prolog.Unifier.Data where

import Autolib.ToDoc (ToDoc(..), vcat, text)
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Data.Generics (gsize)

import qualified Language.Prolog as Pl
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*))

data Unifier = Unifier Pl.Unifier deriving ( Eq, Ord, Typeable, Read, Show )
data Config = Config Pl.Term Pl.Term deriving ( Eq, Ord, Typeable, Read, Show  )

instance Reader Pl.Term where
    reader = do
      input <- getInput
      case parse Pl.term "(term)" input of
         Left err -> fail (show err)
         Right t -> do
                  setInput ""
                  return t

instance Reader Pl.VariableName where
    reader = do
      input <- getInput
      case parse Pl.vname "(term)" input of
         Left err -> fail (show err)
         Right v -> do
                  setInput ""
                  return v

instance Reader Unifier where
    reader = do
      input <- getInput
      case parse (many ((,) <$> Pl.vname <* symbol "=" <*> Pl.term)) "(unifier)" input of
         Left err -> fail (show err)
         Right u -> do
                  setInput ""
                  return (Unifier u)
instance Reader Config where
    reader = do
      input <- getInput
      let [l1,l2] = lines input
      case parse Pl.term "(first term)" l1 of
         Left err -> fail (show err)
         Right t1 -> do
            case parse Pl.term "(second term)" l2 of
               Left err -> fail (show err)
               Right t2 -> do
                  setInput ""
                  return (Config t1 t2)

symbol = between spaces spaces . string

instance ToDoc Unifier where
    toDoc (Unifier u) = vcat $ map text [ show v ++ " = " ++ show t  | (v,t) <- u ])
instance ToDoc Config where
    toDoc (Config t1 t2) = text (unlines [show t1 ++ c1, show t2 ++ c2])

c1 = "    % A config contains exactly two lines, with a Prolog term on each one."
c2 = "    % These are the two terms that are to be unified."

instance Size Unifier where
    size (Unifier u) = length u
instance Size Config where
    size (Config t1 t2) = gsize t1 + gsize t2

