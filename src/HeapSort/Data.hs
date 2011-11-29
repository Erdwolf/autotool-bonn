{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module HeapSort.Data where

import HeapSort.Tree
import HeapSort.Operation

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Data.Derive.All

$(derives [makeEq,makeOrd] [''Operation,''Richtung])

data Feedback = Verbose
              | OnFailure
              | None
   deriving ( Eq, Ord, Typeable)

data QuizConfig = QuizConfig
 { quizFeedback :: Feedback  -- ^ Type of feedback
 , chunks  :: Int            -- ^ Number of chunks during numbers sequence generation (use 1 to deactivate)
 , numbers :: Int            -- ^ Length of the number sequence
 , min :: Int                -- ^ Lower bound for random number generation
 , max :: Int                -- ^ Upper bound for random number generation
 }
   deriving ( Eq, Ord, Typeable)

data Config = Config
 { feedback :: Feedback
 , unsortedNumbers :: [Int]
 }
   deriving ( Eq, Ord, Typeable)

$(derives [makeReader, makeToDoc] [''Operation])

data Solution = Solution [Operation]
   deriving ( Eq, Ord, Typeable, Read, Show )


$(derives [makeReader, makeToDoc] [''Richtung,''Feedback,''QuizConfig,''Config,''Solution])


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
