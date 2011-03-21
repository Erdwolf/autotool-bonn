{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Haskell.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Test.QuickCheck ( Args (..))
import System.Random ( StdGen )

import qualified Language.Haskell.Exts.Parser as Pa
import qualified Language.Haskell.Exts.Pretty as Pr

-- TODO: the ToDoc method should 
-- definitely not print the String quotes
-- and maybe do prettyprinting
-- the Reader Method should not need the string quotes
-- and call a real Haskell parser instead

data Code = Code String deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Code])

data Display = Display 


instance Size Code where
   -- FIXME: should use the size of the syntax tree
    size ( Code cs ) = length cs

code_example :: Code
code_example = Code "\\ n -> (0,n)"

data Driver = QuickCheck Args
            | SmallCheck { tests_run :: Int 
                         , failures_shown :: Int
                         }
   deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Driver])


instance Reader StdGen 
instance ToDoc StdGen

$(derives [makeReader, makeToDoc] [''Args])

data Instance = 
     Instance { global :: Code
              -- | an Haskell expression,
              -- will be applied to student's solution
              -- and must then represent a Quick/Smallcheck property
              , specification :: Code
              , driver :: Driver
              , testing_time_seconds :: Int
              }
     deriving (  Typeable )


$(derives [makeReader, makeToDoc] [''Instance])





instance_example :: Instance
instance_example = Instance
    { global = Code "data Foo = Bar"
    , specification = Code "\\ f -> \\ n -> let (x,y) = f n in if n > 0 then 2^x * y == n && odd y else True "
    , driver = SmallCheck { tests_run = 1000, failures_shown = 10 }
    , testing_time_seconds = 1
    }


