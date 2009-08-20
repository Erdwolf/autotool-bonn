{-# LANGUAGE TemplateHaskell #-}
module Graph.VC.Input where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

import SAT.Types

data Input = Input
	    { formel          :: Formel
	    , anzeige_groesse :: Int	
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Input])
-- {-! for Input derive: Reader, ToDoc !-}

i0 :: Input
i0 = Input { formel = read "(x || y || z) && (! x || y || !z )"
	   , anzeige_groesse = 6 
	   }

-- local variables:
-- mode: haskell
-- end:
