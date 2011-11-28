{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module HeapSort.Data where

import HeapSort.Tree
import HeapSort.Operation

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable


data QuizConfig = QuizConfig
 { quizFeedback :: Bool
 }
   deriving ( Eq, Ord, Typeable)

data Config = Config
 { feedback :: Bool
 , unsortedNumbers :: [Int]
 }
   deriving ( Eq, Ord, Typeable)

data Solution = Solution [Operation]
   deriving ( Eq, Ord, Typeable, Read, Show )

$(derives [makeReader, makeToDoc] [''Operation,''Richtung,''QuizConfig,''Config,''Solution])


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
