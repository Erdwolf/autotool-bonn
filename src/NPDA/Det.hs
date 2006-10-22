module NPDA.Det

--   $Id$

where

import NPDA.Type
import NPDA.Sane

import Control.Monad (guard)
import Autolib.Reporter
import qualified Autolib.Reporter.Checker  as C
import Autolib.Set
import Autolib.ToDoc

check :: NPDAC x y z => C.Type ( NPDA x y z )
check = C.Make 
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
	    let fs =            lookupset (transitionen a) (Just x , z, y)
		     `union` lookupset (transitionen a) (Nothing, z, y)
	    guard $ cardinality fs > 1
	    return ((x, z, y), fs)
    of []  -> inform $ text "das ist ein deterministischer Kellerautomat"
       fss -> reject $ vcat 
	      [ text "das ist kein deterministischer Kellerautomat,"
	      , text "denn für folgende x in X, z in Z, y in Y"
	      , text "enthält  h(x,z,y) + h(epsilon,z,y) mehr als ein Element:"
              , nest 4 $ toDoc fss
	      ]




