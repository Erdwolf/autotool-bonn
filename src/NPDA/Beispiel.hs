module Beispiel where

-- $Log$
-- Revision 1.1  2003-01-13 23:20:05  joe
-- NPDA revamped
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2002/01/13 23:03:33  autotool
-- kellerautomaten: nutze toDoc, akzeptanzmodus sichtbar
--

-- Skript Seite 56

import NPDA
import Akzeptieren
import Set
import ToDoc

import Seite56


-- der Automat soll { w w^R  |  w  in  {0, 1}* }
-- durch leeren Keller akzeptieren

student :: NPDA Char Char Integer
student = NPDA { eingabealphabet = mkSet "01"
	       , kelleralphabet  = mkSet "abc"
	       , zustandsmenge	 = mkSet [ 2, 3 ]
	       , tafel = t
	       , startzustand	 = 2
	       , startsymbol	 = 'a'
	       , akzeptiert	 = Leerer_Keller
	       }


-- die Tabelle direkt aus dem Skript abgeschrieben

t = listToFM [ -- Spalte 1 (Z 2, x = '0')
	       ( ( Just '0' , 2, 'a'), mkSet [ ( 2, "ba") ] )
	     , ( ( Just '0' , 2, 'b'), mkSet [ ( 2, "bb"), ( 3, "") ] )
	     , ( ( Just '0' , 2, 'c'), mkSet [ ( 2, "bc") ] )
	     
	     -- Spalte 2 (Z 2, x = '1')
	     , ( ( Just '1' , 2, 'a'), mkSet [ ( 2, "ca") ] )
	     , ( ( Just '1' , 2, 'b'), mkSet [ ( 2, "cb") ] )
	     , ( ( Just '1' , 2, 'c'), mkSet [ ( 2, "cc"), (3, "") ] )
	     
	     -- Spalte 3 (Z 2, x = epsilon)
	     , ( ( Nothing, 2, 'a'), mkSet [ ( 3, "") ] )

	     -- Spalten 4 bis 6 (Z 3)
	     , ( ( Just '0' , 3, 'b'), mkSet [ ( 3, "" ) ] )
	     , ( ( Just '1' , 3, 'c'), mkSet [ ( 3, "" ) ] )
	     , ( ( Nothing, 3, 'a'), mkSet [ ( 3, "" ) ] )
	     ]


-- durch List Comprehensions läßt sich das kompakter schreiben:

t' = plusFM_C union speichern prüfen

speichern = listToFM 
    [ ( ( Just x, 2, y ), mkSet [ ( 2, [h, y] ) ] )
    | (x, h) <- [ ('0', 'b'), ('1', 'c') ]
    , y <- "abc"
    ]

prüfen	  = listToFM 
    [ ( ( x, z, y ), mkSet [ ( 3, "" ) ] )
    | (x, y) <- [ ( Just '0', 'b'), ( Just '1', 'c'), ( Nothing, 'a') ]
    , z <- [ 2, 3 ]
    ]
