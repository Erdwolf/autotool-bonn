{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module TypeCheckBonn.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Type.Data (Signature, Type, TI(TI))


data Feedback = Detailed
              | YesNo
   deriving ( Eq, Ord, Typeable)

data QuizConfig = QuizConfig
 { quizFeedback :: Feedback  -- ^ Type of feedback
 , maxArity :: Int           -- ^ Maximum arity
 , types :: [String]         -- ^ Available type names
 , minDecls :: Int           -- ^ Minimum number of declarations
 , maxDecls :: Int           -- ^ Maximum number of declarations
 , minSize :: Int            -- ^ Minimum size of solution
 , maxSize :: Int            -- ^ Maximum size of solution
 , seed :: Int               -- ^ Dummy value
 }
   deriving ( Typeable)

data Config = Config
 { feedback :: Feedback
 , targetType :: Type
 , declarations :: Signature
 }
   deriving ( Typeable)

$(derives [makeReader, makeToDoc] [''Feedback,''QuizConfig,''Config])

