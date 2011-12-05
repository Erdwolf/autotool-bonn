{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module EditDistance.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable
import Data.List (transpose, intersperse)
import Control.Applicative ((<$>))
import Control.Monad (liftM)

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

data Solution = Solution Comments [[Int]]
   deriving ( Eq, Ord, Typeable, Read, Show )

type Comments = (String, String) -- The two input strings


$(derives [makeReader, makeToDoc] [''Feedback,''StringGen,''ErrorType,''QuizConfig,''Config])

instance Reader Solution where
    reader = do
        my_symbol "["
        (xss,t) <- liftM unzip $
                   my_commaSep $ do
                       my_symbol "["
                       xs <- my_commaSep reader
                       string "] -- "
                       c <- anyChar
                       return xs
        string "]-- "
        s <- anyChar `Autolib.Reader.sepBy` my_whiteSpace
        return $ Solution (s,t) (transpose xss)

instance ToDoc Solution where
    -- The field should always be shown in rectangular form.
    toDoc (Solution (s,t) xss) =
        vcat (zipWith (<+>) (text "[": repeat (text ","))
                            (zipWith  (<+>) (map toDoc $ transpose xss)
                                            ([ text "--" <+> text [chr] | chr <- t ] ++ repeat empty))
             ) $$ (text "]" <> if null s then empty else text "-- " <> hcat (intersperse (text "   ") [ text [chr] | chr <- s ]))
        --hsep (map (vcat . map (text . show)) xss)


instance Size Config   where size _ = 0
instance Size Solution where size _ = 0
