module Inter.Boiler where

-- $Id$

import Inter.Types

import qualified LOOP.SQRT
import qualified LOOP.TIMES
import qualified LOOP.PRIM
import qualified LOOP.FIB

import qualified TM.FACTOR


-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence 
       [ fmap Variant $ LOOP.SQRT.generate
       , fmap Variant $ LOOP.TIMES.generate
       , fmap Variant $ LOOP.PRIM.generate
       , fmap Variant $ LOOP.FIB.generate

       , fmap Variant $ TM.FACTOR.generate
       ]

