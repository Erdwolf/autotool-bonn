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


data ArrayBonn = ArrayBonn deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore ArrayBonn where
    scoringOrder _ = None -- ?

instance Class ArrayBonn Statement Program.ArrayBonn.Value.Value where
    execute p = Program.ArrayBonn.Semantics.execute
    example p = ( Program [ Program.ArrayBonn.Statement.s0 ]
                , Program.GeneralBonn.Environment.make 
                      [ ( read "x", Program.ArrayBonn.Value.example ) ]
                )

make_quiz :: Make
make_quiz = quiz ArrayBonn Quiz.example

data InstanceConfig = InstanceConfig
    { feedback :: Bool
    , initial :: Environment Program.ArrayBonn.Value.Value
    , program :: Program Statement
    , final   :: Environment Program.ArrayBonn.Value.Value
    }
  deriving Typeable

$(derives [makeReader, makeToDoc] [''InstanceConfig])


