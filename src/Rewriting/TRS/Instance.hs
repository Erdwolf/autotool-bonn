{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, UndecidableInstances, TemplateHaskell #-}

module Rewriting.TRS.Instance where

import Rewriting.TRS

import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap

import Data.Typeable



data (  Symbol c, Symbol v
       , ToDoc ( TRS v c )
       , Reader ( TRS v c )
--     , Eq ( TRS v c ), Ord ( TRS v c ) 
--     , Eq ( Term v c) , Ord ( Term v c )
     ) 
    => Instance v c = Instance
        { system :: TRS v c
        , from   :: Term v c
        , to     :: Term v c
        }
    deriving ( Typeable 
	     -- , Eq, Ord
	     )

example :: Instance Identifier Identifier
example = Instance
        { system = Rewriting.TRS.example
        , from = read "f(a,f(a,b))"
        , to = read "f(f(b,a),a)"
        }

$(derives [makeReader, makeToDoc] [''Instance])
-- {-! for Instance derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end:
