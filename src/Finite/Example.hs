module Finite.Example where

import Finite.Type
import Finite.Run
import Finite.Wort

import Set

prog :: Program Question Action
-- soll alle wörter aus a^* b a^* erkennen
prog = Program 
    { statements =
	       [ Label "start" 
	       , Assign "count-b" (Const "0") 
	       , Label "repeat" 
	       , If ( Ask (Exists R), Const "false" ) "final" 
	       , If ( Ask Read, Const "a" ) "read-a"
	       , If ( Ask Read, Const "b" ) "read-b"
	       , Label "reject" 
	       , Reject -- sonst falscher Buchstabe
	       , Label "read-a" 
	       , Do ( Go R ) 
	       , Goto "repeat"
	       , Label "read-b" 
	       , If ( Ref "count-b", Const "1" ) "reject" 
	       , Assign "count-b" (Const "1")
	       , Do (Go R)
	       , Goto "repeat"
	       , Label "final" 
	       , If ( Ref "count-b", Const "1" ) "accept" 
	       , Reject
	       , Label "accept" 
	       , Accept
	       ]
    , trace = emptySet
    , watch = mkSet [ "count-b" ]
    }

