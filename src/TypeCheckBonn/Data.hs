{-# LANGUAGE OverlappingInstances, TemplateHaskell, DeriveDataTypeable, StandaloneDeriving #-}
module TypeCheckBonn.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Type.Tree (ToTree (toTree))
import qualified Data.Tree as T

import Data.Typeable
import Control.Monad (liftM)

import Type.Data (Signature, Type, TI (TI))

import Autolib.TES.Term (Term (Node))
import Autolib.TES.Identifier (Identifier)


data Feedback = Detailed
              | YesNo
   deriving (Eq, Ord, Typeable)

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
   deriving (Typeable)

data Config = Config
 { feedback :: Feedback
 , targetType :: Type
 , declarations :: Signature
 }
   deriving (Typeable)

$(derives [makeReader, makeToDoc] [''Feedback,''QuizConfig,''Config])

data Exp = Var Identifier
         | Call Identifier [Exp]
      deriving (Typeable)

instance Reader Exp where
    reader = convert `liftM` reader

convert :: Term Identifier Identifier -> Exp
convert (Node n [])   = Var n
convert (Node n args) = Call n (map convert args)

instance ToDoc Exp where
    toDoc (Var n)       = toDoc n
    toDoc (Call n args) = toDoc n <> (parens $ vcat $ punctuate (text ",") $ map toDoc args)

instance ToTree Exp where
    toTree = T.unfoldTree uf
       where
         uf (Var n )      = (show n, [])
         uf (Call n args) = (show n, args)
