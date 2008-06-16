module Robots3.Examples where

-- -- $Id$

import Robots3.Data
import Robots3.Config

import Autolib.ToDoc

-- | Aufgabe von Alexander Krabbes
alex :: Config 
alex = make
  [ Robot { name = "A", position = Position { x = 4, y = 5} }
  , Robot { name = "B", position = Position { x = -4,y =  5} }
  , Robot { name = "C", position = Position { x = 3, y = 4} }
  , Robot { name = "D", position = Position { x = -3,y =  4} }
  , Robot { name = "E", position = Position { x = 0, y = 5} }
  , Robot { name = "F", position = Position { x = 4,y = -4} }
  , Robot { name = "G", position = Position { x = -4,y = -4} }
  ]
  [ Position {x = 0, y = 0} ]

-- | die beispielkarte nr. 40 aus dem original-spiel
-- von binaryarts.com
fourty :: Config
fourty = make
  [ Robot { name = "A", position = Position { x = -2, y = 2} }
  , Robot { name = "B", position = Position { x = 0,y =  2} }
  , Robot { name = "C", position = Position { x = 2, y = 2} }
  , Robot { name = "D", position = Position { x = 2,y = -1} }
  , Robot { name = "E", position = Position { x = -1,y = -2} }
  ]
  [ Position { x = 0, y = 0} ]

