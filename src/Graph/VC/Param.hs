{-# LANGUAGE TemplateHaskell #-}

module Graph.VC.Param where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data Param = Param
	    { knoten               :: Int
	    , kanten               :: Int
	    , deck_knoten_moeglich :: Int
	    , deck_knoten_maximum  :: Int
	    , kanten_in_deckmenge  :: Int
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Param])

p0 :: Param
p0 = Param { knoten               = 15
	   , kanten               = 25
	   , deck_knoten_moeglich = 6
	   , deck_knoten_maximum  = 8
	   , kanten_in_deckmenge  = 3
	   }
