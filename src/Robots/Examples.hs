module Robots.Examples where

-- -- $Id$

import Robots.Data
import Robots.Konfig

import Inter.Types
import Robots.Interface ( make )

generates :: [ IO Variant ]
generates = map ( return . Variant )
	  [ make "R" "Alex" alex
	  , make "R" "Fourty" fourty
	  ]

alex :: Konfig 
-- aufgaben von Alexander Krabbes
alex = mkKonfig
		  [ Robot { name = "A", position = ( 4, 5), ziel = Just (0,0) }
		  , Robot { name = "B", position = (-4, 5), ziel = Nothing }
		  , Robot { name = "C", position = ( 3, 4), ziel = Nothing }
		  , Robot { name = "D", position = (-3, 4), ziel = Nothing }
		  , Robot { name = "E", position = ( 0, 5), ziel = Nothing }
		  , Robot { name = "F", position = ( 4,-4), ziel = Nothing }
		  , Robot { name = "G", position = (-4,-4), ziel = Nothing }
		  ]

fourty :: Konfig
-- die beispielkarte nr. 40 aus dem original-spiel
fourty = mkKonfig
		  [ Robot { name = "A", position = (-2, 2), ziel = Nothing }
		  , Robot { name = "B", position = ( 0, 2), ziel = Nothing }
		  , Robot { name = "C", position = ( 2, 2), ziel = Nothing }
		  , Robot { name = "D", position = ( 2,-1), ziel = Nothing }
		  , Robot { name = "E", position = (-1,-2), ziel = Just (0,0) }
		  ]
