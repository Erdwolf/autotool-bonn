{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import Syntax.Central
import Syntax.Syntax
import Syntax.Data
import Syntax.Generator

instance Generator Syntax () Config where
    generator p () key = do -- IO
      Config 4 <$> Syntax.Generator.generate


instance Project Syntax Config Config where
    project p = id



make :: Make
make = quiz Syntax ()
