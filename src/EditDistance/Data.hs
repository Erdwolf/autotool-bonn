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


data StringGen = Length Int -- ^ Generate  random string of a certain length.
               | DeleteReplaceInsert Int Int Int
                  -- ^ Use certain numbers of deletions, replacements, and insertions
                  --   (in that order) to generate the string from the other one.
   deriving ( Eq, Ord, Typeable)

data QuizConfig = QuizConfig
 { quizFeedback    :: Feedback  -- ^ Type of feedback
 , quizMaxErrors   :: Int       -- ^ Allowed number of errors
 , lengthOfstring1 :: Int       -- ^ Length of first string
 , generateString2 :: StringGen -- ^ How to generate the second string
 , characters      :: Int       -- ^ Number of different characters
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


$(derives [makeReader, makeToDoc] [''Feedback,''StringGen,''QuizConfig,''Config,''Solution])


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
