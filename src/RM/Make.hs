module RM.Make where

import Inter.Types

import RM.Type
import RM.Check
import RM.Machine

import qualified Machine.Numerical.Config as C
import qualified Machine.Numerical.Make as M
import qualified Machine.Numerical.Inter as I

import qualified Autolib.Reporter.Checker as R

make :: Make
make = M.make $ C.Config
       { C.name = "RM"
       , C.conditions = 
	 [ "Die Eingabe erfolgt in den Registern 1 und 2 und"
	 , "die Ausgabe soll im Register 0 stehen."
	 , "Die Werte der Eingaberegister sollen erhalten bleiben."
	 ]
       , C.arity = 2
       , C.op = read "x1 + x2"
       , C.num_args = 10
       , C.max_arg = 20
       , C.cut = 1000
       , C.checks = [ NumReg 4 ]
       , C.start = read "(s4)4" :: Program
       }
