{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- , IncoherentInstances #-}

module Program.GeneralBonn.Class where

import Program.GeneralBonn.Environment
import Program.GeneralBonn.Program

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

