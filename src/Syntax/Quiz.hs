{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable #-}
module Syntax.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))

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
