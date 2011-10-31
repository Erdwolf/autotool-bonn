{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable #-}
module Syntax.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import Syntax.Central
import Syntax.Syntax
import Syntax.Data
import Syntax.Generator

import Data.Typeable


data QuizConfig = QuizConfig
    { feedback :: Bool
    }
  deriving ( Eq, Ord, Typeable)

$(derives [makeReader, makeToDoc] [''QuizConfig])


instance Generator Syntax () Config where
    generator p (QuizConfig fb) key = do -- IO
      Config fb 4 <$> Syntax.Generator.generate


instance Project Syntax Config Config where
    project p = id



make :: Make
make = quiz Syntax ()
