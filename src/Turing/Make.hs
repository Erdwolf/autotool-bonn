module Turing.Make 

( computer
, acceptor
)

where

--  $Id$

import Inter.Types
import Inter.Quiz

import Turing.Property
import Turing.Type
import qualified Turing.Machine
import qualified Turing.Example
import qualified Turing.Config as TC

import qualified Machine.Acceptor.Type as A
import qualified Machine.Acceptor.Inter
import Language.Type
import Language.Inter

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I
import qualified Autolib.Reporter.Checker as R

import Autolib.Set
import Autolib.ToDoc
import Autolib.Informed
import Autolib.Util.Zufall


computer :: Make
computer = M.make $ C.Config
     { C.name = "Turing-Maschine (als Computer)"
     , C.conditions = []
     , C.arity = 2
     , C.op = read "x1 * x2" 
     , C.num_args = 10
     , C.max_arg = 20
     , C.cut = 1000
     , C.checks = [ Sane ]
     , C.start = Turing.Example.student
     }

----------------------------------------------------------------

acceptor :: Make
acceptor = quiz ( A.Acceptor "Turing" ) TC.example

type Accept = A.Type ( Turing Char Int ) String Property

instance Project A.Acceptor Accept Accept where
    project _ i = i

instance Generator A.Acceptor TC.Config Accept where
    generator _ config key = do
        let l = inter $ TC.lang config
            m = TC.max_num config
            e = TC.max_length config
            small = \ w -> length w <= e
        yeah <- lift $ samples      l m 0
        noh  <- lift $ anti_samples l m 0
        return $ A.Make
           { A.machine_desc = text "Turing-Maschine (als Akzeptor)"
           , A.data_desc = info $ TC.lang config
           , A.yeah = take m $ filter small yeah
           , A.noh  = take m $ filter small noh
           , A.cut  = TC.cut config
           , A.properties = Sane : TC.properties config
           , A.start = TC.start config
           }
