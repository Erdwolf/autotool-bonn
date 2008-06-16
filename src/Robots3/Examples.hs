module Robots3.Examples where

-- -- $Id$

import Robots3.Data
import Robots3.Config

import Autolib.ToDoc

-- | Aufgabe von Alexander Krabbes
alex :: Config 
alex = make
  [ Robot { name = "A", position = ( 4, 5) }
  , Robot { name = "B", position = (-4, 5) }
  , Robot { name = "C", position = ( 3, 4) }
  , Robot { name = "D", position = (-3, 4) }
  , Robot { name = "E", position = ( 0, 5) }
  , Robot { name = "F", position = ( 4,-4) }
  , Robot { name = "G", position = (-4,-4) }
  ]
  [ (0,0) ]

-- | die beispielkarte nr. 40 aus dem original-spiel
-- von binaryarts.com
fourty :: Config
fourty = make
  [ Robot { name = "A", position = (-2, 2) }
  , Robot { name = "B", position = ( 0, 2) }
  , Robot { name = "C", position = ( 2, 2) }
  , Robot { name = "D", position = ( 2,-1) }
  , Robot { name = "E", position = (-1,-2) }
  ]
  [ (0,0) ]

