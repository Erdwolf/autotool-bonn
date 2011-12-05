{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module EditDistance.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Data.List (transpose)

data Feedback = WrongEntries
              | NumberOfErrorsWithCutoffAt Int
              | None
   deriving ( Eq, Ord, Typeable)

data StringGen = Length Int -- ^ Generate  random string of a certain length.
               | DeleteReplaceInsert Int Int Int
                  -- ^ Use certain numbers of deletions, replacements, and insertions
                  --   (in that order) to generate the string from the other one.
   deriving ( Eq, Ord, Typeable)

data ErrorType = WrongNumbers    -- ^ The number in the submitted and the correct table are compared..
               | Miscalculations -- ^ For each entry in the submitted table, it is checked whether the number was correctly calculated from its dependents.
   deriving ( Eq, Ord, Typeable)

data QuizConfig = QuizConfig
 { quizFeedback    :: Feedback  -- ^ Type of feedback
 , quizErrorType   :: ErrorType -- ^ What counts as an error?
 , lengthOfstring1 :: Int       -- ^ Length of first string
 , generateString2 :: StringGen -- ^ How to generate the second string
 , characters      :: Int       -- ^ Number of different characters
 }
   deriving ( Eq, Ord, Typeable)

data Config = Config
 { feedback  :: Feedback  -- ^ Type of feedback
 , errorType :: ErrorType -- ^ What counts as an error?
 , string1   :: String
 , string2   :: String
 }
   deriving ( Eq, Ord, Typeable)

data Solution = Solution [[Int]]
   deriving ( Eq, Ord, Typeable, Read, Show )


$(derives [makeReader, makeToDoc] [''Feedback,''StringGen,''ErrorType,''QuizConfig,''Config])

instance Reader Solution where
    reader = (Solution . transpose) <$> reader

instance ToDoc Solution where
    -- The field should always be shown in rectangular form.
    toDoc (Solution xss) =
        toDoc (transpose xss)
        --hsep (map (vcat . map text) xss)


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
