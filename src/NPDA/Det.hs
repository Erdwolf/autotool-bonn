module NPDA.Det

-- $Id$

where

import NPDA.Type

import Monad (guard)
import Reporter
import Set
import ToDoc

ist_deterministisch 
    :: ( Ord x, Ord y, Ord z
       , Show x, Show y, Show z
       , ToDoc x, ToDoc y, ToDoc z
       , ToDoc [y]
       )
    => NPDA x y z -> Reporter ()
ist_deterministisch a = 
    case do x <- setToList $ eingabealphabet a
	    z <- setToList $ zustandsmenge a
	    y <- setToList $ kelleralphabet a
	    let fs =            lookupset (tafel a) (Just x , z, y)
		     `union` lookupset (tafel a) (Nothing, z, y)
	    guard $ cardinality fs > 1
	    return ((x, z, y), setToList fs)
    of []  -> inform $ text "das ist ein deterministischer Kellerautomat"
       fss -> reject $ vcat 
	      [ text "das ist kein deterministischer Kellerautomat,"
	      , text "denn für folgende x in X, z in Z, y in Y"
	      , text "enthält  h(x,z,y) + h(epsilon,z,y) mehr als ein Element:"
              , nest 4 $ toDoc fss
	      ]




