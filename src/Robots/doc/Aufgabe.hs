import Challenger
import Robots.Type

student = Aufgabe 
	{ problem = Robots
	, instanz = mkKonfig
		  [ Robot { name = "A", position = (-2, 2), ziel = Nothing }
		  , Robot { name = "B", position = ( 0, 2), ziel = Nothing }
		  , Robot { name = "C", position = ( 2, 2), ziel = Nothing }
		  , Robot { name = "D", position = ( 2,-1), ziel = Nothing }
		  , Robot { name = "E", position = (-1,-2), ziel = Just (0,0) }
		  ]
	, beweis = [ ( "B", W ) ]
	}
