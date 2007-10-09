{-# OPTIONS -Onot #-}

module Graph.VC.ParamSAT where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import qualified SAT.Param

data Param = Param
	    { formel          :: SAT.Param.Param
	    , anzeige_groesse :: Int	
	    }
     deriving ( Typeable )

{-! for Param derive: Reader, ToDoc !-}

p0 :: Param
p0 = Param { formel          = SAT.Param.p 5
	   , anzeige_groesse = 10 
	   }

-- local variables:
-- mode: haskell
-- end:
