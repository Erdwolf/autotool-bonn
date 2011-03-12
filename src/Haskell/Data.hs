{-# LANGUAGE TemplateHaskell #-}

module Haskell.Data where

import Autolib.ToDoc
import Autolib.Reader

-- TODO: the ToDoc method should 
-- definitely not print the String quotes
-- and maybe do prettyprinting
-- the Reader Method should not need the string quotes
-- and call a real Haskell parser instead

data Code = Code String

$(derives [makeReader, makeToDoc] [''Code])

code_example :: Code
code_example = Code "id"

data Instance = 
     Instance { description :: String
              -- | an Haskell expression,
              -- representing a Quick/Smallcheck property
              , specification :: Code
              }

$(derives [makeReader, makeToDoc] [''Instance])

instance_example :: Instance
instance_example = Instance
    { description = ""
    , specification = Code "\\ xs -> f ( reverse xs ) == xs"
    }


