{-# language DeriveDataTypeable #-}

module Diffie_Hellman.Config where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

data Config = 
     Config { public :: Public
            , private:: Private
            }
     deriving Typeable

example :: Config
example = Config 
        { public = Public 
                 { p = 31 , g = 3, g_a = 28, g_b = 13 }
        , private = Private
                  { a = 16, b = 11, g_ab = 18 }
        }

{-! for Config derive: Reader, ToDoc !-}

data Private =
     Private { a :: Integer
            , b :: Integer
            , g_ab :: Integer
            }
     deriving Typeable


{-! for Private derive: Reader, ToDoc !-}

data Public =
     Public { p :: Integer
            , g :: Integer
            , g_a :: Integer
            , g_b :: Integer
            }
    deriving Typeable

{-! for Public derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
