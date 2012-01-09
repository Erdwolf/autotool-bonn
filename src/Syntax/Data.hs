{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module Syntax.Data where

import Syntax.Syntax
import Syntax.Checker

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

deriving instance Ord Graph

data QuizConfig = QuizConfig
 { quizFeedback :: Bool
 , seed :: Int
 }
   deriving ( Eq, Ord, Typeable)

data Config = Config
 { feedback :: Bool
 , expectedNumberOfWords :: Int
 , language :: Language
 }
   deriving ( Eq, Ord, Typeable)

data Solution = Solution [String]
   deriving ( Eq, Ord, Typeable, Read, Show )

$(derives [makeReader, makeToDoc] [''QuizConfig,''Config,''Solution,''Graph])


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
