module Inter.Boiler where

-- $Id$

import Inter.Types


-- vorf�hren:

import qualified Demo.L


-- ws03/aus NFA 
import qualified Serie1
import qualified Serie2

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence $
       -- der erste ist der default-wert!

       [ 
       , Serie2.generate

       ]
	   ++ Serie2.generates

       ++ Demo.L.generates

