module NPDA.Det

-- $Id$

where

import NPDA.Type
import NPDA.Sane

import Monad (guard)
import Reporter
import qualified Reporter.Checker  as C
import Set
import ToDoc

check :: NPDAC x y z => C.Type ( NPDA x y z )
check = sanity `C.and_then` C.Make 
      { C.nametag = "det"
      , C.condition = text "Der Kellerautomat soll determinstisch sein."
      , C.investigate = ist_deterministisch
      }

ist_deterministisch 
    :: NPDAC x y z
    => NPDA x y z -> Reporter ()
ist_deterministisch a = 
    case do x <- setToList $ eingabealphabet a
	    z <- setToList $ zustandsmenge a
	    y <- setToList $ kelleralphabet a
	    let fs =            lookupset (tafel a) (Just x , z, y)
		     `union` lookupset (tafel a) (Nothing, z, y)
	    guard $ cardinality fs > 1
	    return ((x, z, y), fs)
    of []  -> inform $ text "das ist ein deterministischer Kellerautomat"
       fss -> reject $ vcat 
	      [ text "das ist kein deterministischer Kellerautomat,"
	      , text "denn für folgende x in X, z in Z, y in Y"
	      , text "enthält  h(x,z,y) + h(epsilon,z,y) mehr als ein Element:"
              , nest 4 $ toDoc fss
	      ]




