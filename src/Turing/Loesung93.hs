module Loesung93 where


-- import Aufgabe93
import Turing

import ToDoc
import Reader
import Dot.Dot

import Turing.Z

-- die Maschine soll { I ^ (a * b) | a, b > 1 } akzeptieren


student :: Turing Char Z
student = Turing 
	{ eingabealphabet = mkSet "I"
	, leerzeichen	  = '#'
	, arbeitsalphabet = mkSet "#abI"
	, zustandsmenge	 = mkSet [ A0, A1, A2, B, C, D, E, F ]
	, tafel = t
	, startzustand	 = A0
	, endzustandsmenge = mkSet [ F ]
	}


t = listToFM
      [ ( ('I', A0), mkSet [ ( 'a', A1, R ) ] )
      , ( ('I', A1), mkSet [ ( 'a', A2, R ) ] )
      , ( ('I', A2), mkSet [ ( 'a', A2, R ), ('I', B, L) ] )

      , ( ('a', B), mkSet [ ('a', B, L) ] )
      , ( ('b', B), mkSet [ ('b', B, L) ] )
      , ( ('#', B), mkSet [ ('#', C, R) ] )

      , ( ('a', C), mkSet [ ('#', D, R) ] )
      , ( ('b', C), mkSet [ ('a', E, R) ] )

      , ( ('a', D), mkSet [ ('a', D, R) ] )
      , ( ('b', D), mkSet [ ('b', D, R) ] )
      , ( ('I', D), mkSet [ ('b', B, L) ] )

      , ( ('b', E), mkSet [ ('a', E, R) ] )
      , ( ('I', E), mkSet [ ('I', B, L) ] )
      , ( ('#', E), mkSet [ ('#', F, O) ] )
      ]

