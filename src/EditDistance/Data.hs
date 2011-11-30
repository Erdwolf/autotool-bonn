{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module EditDistance.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Feedback = WrongEntries
              | NumberOfErrors
              | None
   deriving ( Eq, Ord, Typeable)

data QuizConfig = QuizConfig
 { quizFeedback  :: Feedback  -- ^ Type of feedback
 , quizMaxErrors :: Int       -- ^ Allowed number of errors
 , stringLength1 :: Int       -- ^ Length of first string
 , stringLength2 :: Int       -- ^ Length of second string
 , characters    :: Int       -- ^ Number of different characters
 }
   deriving ( Eq, Ord, Typeable)

data Config = Config
 { feedback  :: Feedback
 , maxErrors :: Int
 , string1   :: String
 , string2   :: String
 }
   deriving ( Eq, Ord, Typeable)

data Solution = Solution [[Int]]
   deriving ( Eq, Ord, Typeable, Read, Show )


$(derives [makeReader, makeToDoc] [''Feedback,''QuizConfig,''Config,''Solution])


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
