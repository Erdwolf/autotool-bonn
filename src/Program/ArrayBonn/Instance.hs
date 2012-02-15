{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, TemplateHaskell #-}

module Program.ArrayBonn.Instance where

import Program.ArrayBonn.Statement
import Program.ArrayBonn.Value
import Program.ArrayBonn.Semantics

import qualified Program.ArrayBonn.Roll as R
import qualified Program.ArrayBonn.Config as Quiz

import Program.GeneralBonn.Class
import Program.GeneralBonn.Environment
import Program.GeneralBonn.Program

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import qualified Challenger as C

import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Util.Zufall ( repeat_until )

import Data.Typeable
import Data.Maybe ( isNothing, isJust )



