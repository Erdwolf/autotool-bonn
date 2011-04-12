{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Haskell.Blueprint.Data where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

import Test.QuickCheck ( Args (..))
import System.Random ( StdGen )

data Code = Code String deriving ( Eq, Ord, Typeable )

instance Reader Code where 
    reader = do cs <- getInput ; setInput "" ; return $ Code cs

instance ToDoc Code where 
    toDoc ( Code cs ) = vcat $ map text $ lines cs

-- FIXME: should use the size of the syntax tree
instance Size Code where
    size ( Code cs ) = length cs

code_example :: Code
code_example = Code $ unlines 
    [ "import Test.SmallCheck"
    , "foo :: Int"
    , "foo = undefined"
    , ""
    , "test :: Bool"
    , "test = foo * foo == 9"
    ]


-- below here: not needed?

data Driver = QuickCheck Args
            | SmallCheck { tests_run :: Int 
                         , failures_shown :: Int
                         }
   deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Driver])


instance Reader StdGen 
instance ToDoc StdGen

$(derives [makeReader, makeToDoc] [''Args])



