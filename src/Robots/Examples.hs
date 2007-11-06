module Robots.Examples where

-- -- $Id$

import Robots.Data
import Robots.Config

import Autolib.ToDoc

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

e27 :: Config
e27 = make [ Robot
           { name = "A" , position = ( 1 , -5 ) , ziel = Nothing }
     , Robot
           { name = "B" 
           , position = ( 1, -4) -- ( 1, 4)  -- (-5, 4) -- ( -5 , -5 )
           , ziel = Just
                        ( -1 , -2 )
           }
     , Robot
           { name = "C" , position = ( -6 , 4 ) , ziel = Nothing }
     , Robot
           { name = "D" , position = ( -5 , -6 ) , ziel = Nothing }
     , Robot
           { name = "E" , position = ( 4 , -5 ) , ziel = Nothing }
     , Robot
           { name = "F" , position = ( 1 , 5 ) , ziel = Nothing }
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


c1 = make 
     [ Robot
           { name = "A" , position = ( -4 , 6 )
           , ziel = Just
                        ( 0 , 0 )
           }
     , Robot
           { name = "B" , position = ( -6 , 6 ) , ziel = Nothing }
     , Robot
           { name = "C" , position = ( 6 , 6 ) , ziel = Nothing }
     , Robot
           { name = "D" , position = ( -5 , -7 ) , ziel = Nothing }
     , Robot
           { name = "E" , position = ( 4 , -7 ) , ziel = Nothing }
     , Robot
           { name = "F" , position = ( -6 , -5 ) , ziel = Nothing }
     , Robot
           { name = "G" , position = ( 3 , 7 ) , ziel = Nothing }
     ]

c2 = make [ Robot
           { name = "A" , position = ( 1 , 7 )
           , ziel = Just
                        ( 0 , 0 )
           }
     , Robot
           { name = "B" , position = ( -2 , 8 ) , ziel = Nothing }
     , Robot
           { name = "C" , position = ( 8 , -4 ) , ziel = Nothing }
     , Robot
           { name = "D" , position = ( 1 , -7 ) , ziel = Nothing }
     , Robot
           { name = "E" , position = ( 0 , -8 ) , ziel = Nothing }
     , Robot
           { name = "F" , position = ( -2 , -8 ) , ziel = Nothing }
     , Robot
           { name = "G" , position = ( 5 , 8 ) , ziel = Nothing }
     ]

c3 = make [ Robot
           { name = "A" , position = ( -12 , 10 )
           , ziel = Just
                        ( 0 , 0 )
           }
     , Robot
           { name = "B" , position = ( 12 , 11 ) , ziel = Nothing }
     , Robot
           { name = "C" , position = ( -12 , -10 ) , ziel = Nothing }
     , Robot
           { name = "D" , position = ( 11 , -10 ) , ziel = Nothing }
     , Robot
           { name = "E" , position = ( 12 , -3 ) , ziel = Nothing }
     , Robot
           { name = "F" , position = ( 11 , 10 ) , ziel = Nothing }
     , Robot
           { name = "G" , position = ( 12 , 10 ) , ziel = Nothing }
     ]
