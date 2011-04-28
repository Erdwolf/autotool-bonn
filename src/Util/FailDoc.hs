{-# LANGUAGE TypeSynonymInstances #-}
module Util.FailDoc where

import Autolib.Reporter
import Autolib.ToDoc

import Text.ParserCombinators.Parsec

class Monad m => FailDoc m where
    failDoc :: Doc -> m a

instance FailDoc (GenParser tok st) where
    failDoc = fail . show

instance FailDoc Reporter where
    failDoc = reject


