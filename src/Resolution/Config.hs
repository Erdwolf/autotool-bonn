{-# language DeriveDataTypeable #-}

module Resolution.Config where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

data Target = Empty | Random 
    deriving ( Eq, Ord, Typeable )

{-! for Target derive : Reader, ToDoc !-}

data Config =
     Config { num_variables :: Int
            , literals_per_clause_bounds :: (Int, Int)
            , target :: Target
            }
    deriving ( Typeable )

{-! for Config derive : Reader, ToDoc !-}

example :: Config
example = Config
    { num_variables = 5
    , literals_per_clause_bounds = ( 2, 3 )
    , target = Empty
    }

-- local variables:
-- mode: haskell
-- end:
