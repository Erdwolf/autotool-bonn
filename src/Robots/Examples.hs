module Robots.Examples where

-- -- $Id$

import Robots.Data
import Robots.Config

-- | Aufgabe von Alexander Krabbes
alex :: Config 
alex = make
  [ Robot { name = "A", position = ( 4, 5), ziel = Just (0,0) }
  , Robot { name = "B", position = (-4, 5), ziel = Nothing }
  , Robot { name = "C", position = ( 3, 4), ziel = Nothing }
  , Robot { name = "D", position = (-3, 4), ziel = Nothing }
  , Robot { name = "E", position = ( 0, 5), ziel = Nothing }
  , Robot { name = "F", position = ( 4,-4), ziel = Nothing }
  , Robot { name = "G", position = (-4,-4), ziel = Nothing }
  ]

-- | die beispielkarte nr. 40 aus dem original-spiel
-- von binaryarts.com
fourty :: Config
fourty = make
  [ Robot { name = "A", position = (-2, 2), ziel = Nothing }
  , Robot { name = "B", position = ( 0, 2), ziel = Nothing }
  , Robot { name = "C", position = ( 2, 2), ziel = Nothing }
  , Robot { name = "D", position = ( 2,-1), ziel = Nothing }
  , Robot { name = "E", position = (-1,-2), ziel = Just (0,0) }
  ]

-- | selbst gewürfelte:
e13 :: Config
e13 =  make [ Robot { name = "A", position = ( 1, -1 )
               , ziel = Nothing
               }
       , Robot { name = "B", position = ( -2, 3 ), ziel = Nothing }
       , Robot { name = "C", position = ( -3, -2 ), ziel = Just ( 0, 0 ) }
       , Robot { name = "D", position = ( -1, -2 ), ziel = Nothing }
       , Robot { name = "E", position = ( 0, 3 ), ziel = Nothing }
       ]

e10 :: Config
e10 = make
    [ Robot { name = "A", position = ( 2, -1 )
              , ziel = Just ( 0, 0 )
              }
      , Robot { name = "B", position = ( 1, 1 ), ziel = Nothing }
      , Robot { name = "C", position = ( -2, -1 ), ziel = Nothing }
      , Robot { name = "D", position = ( -3, 0 ), ziel = Nothing }
      , Robot { name = "E", position = ( -1, -1 ), ziel = Nothing }
      ]

