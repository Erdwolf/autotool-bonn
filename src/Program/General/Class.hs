{-# language MultiParamTypeClasses #-}

module Program.General.Class where

import Program.General.Environment

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

class ( Reader st, ToDoc st , Reader val, ToDoc val ) 
        => Class st val where
    single :: Environment val -> st -> Reporter ( Environment val )

