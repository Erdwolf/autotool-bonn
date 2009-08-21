{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Resolution.Config where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Target = Empty | Random 
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Target])

data Config =
     Config { num_variables :: Int
            , literals_per_clause_bounds :: (Int, Int)
            , target :: Target
            }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])

example :: Config
example = Config
    { num_variables = 5
    , literals_per_clause_bounds = ( 2, 3 )
    , target = Empty
    }

-- local variables:
-- mode: haskell
-- end:
