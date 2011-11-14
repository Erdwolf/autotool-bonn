{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- , IncoherentInstances #-}

module Program.General.Class where

import Program.General.Environment
import Program.General.Program

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types

import Data.Typeable

class ( Read p, Show p, Typeable p
      , Reader st, ToDoc st , Typeable st
      , Value val 
      , OrderScore p
      ) 
        => Class p st val | p -> st, p -> val where
    execute :: p -> Environment val -> Program st -> Reporter ( Environment val )
    example :: p -> ( Program st, Environment val )

