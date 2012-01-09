{-# LANGUAGE MultiParamTypeClasses #-}
module Syntax.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import Syntax.Central
import Syntax.Syntax
import Syntax.Data
import Syntax.Generator

import Data.Typeable



instance Generator Syntax QuizConfig Config where
    generator p (QuizConfig fb _ n cfg) key = do -- IO
      Config fb n <$> Syntax.Generator.generate' (cfg { gc_requiredWords = n `max` gc_requiredWords cfg })


instance Project Syntax Config Config where
    project p = id



make :: Make
make = quiz Syntax (QuizConfig True 1 (gc_requiredWords cfg) cfg)
 where cfg = defaultConfig
