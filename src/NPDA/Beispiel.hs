module NPDA.Beispiel where

--   $Id$

-- Skript Seite 56

import NPDA.Type
import NPDA.Dot

-- import Akzeptieren
import Autolib.Set
import Autolib.ToDoc

-- import Seite56

anbn :: NPDA Char Char Int
anbn = NPDA 
     { eingabealphabet = mkSet "ab"
     , kelleralphabet = mkSet "XA" , zustandsmenge = mkSet [ 0, 1 ]
     , startzustand = 0 , startsymbol = 'X' , akzeptiert = Leerer_Keller
     , transitionen = listToFM [ ( ( Just 'a', 0 , 'X'), mkSet [ ( 0, "AX")])
			, ( ( Just 'a', 0 , 'A'), mkSet [ ( 0, "AA")])
                        , ( ( Just 'b', 0 , 'A'), mkSet [ ( 1, "") ])
                        , ( ( Just 'b', 1 , 'A'), mkSet [ ( 1, "") ])
			]
     }

-- | der Automat soll { w w^R  |  w  in  {a, b}* }
-- durch leeren Keller akzeptieren

student :: NPDA Char Char Int
student = NPDA { eingabealphabet = mkSet "ab"
	       , kelleralphabet  = mkSet "ABC"
	       , zustandsmenge	 = mkSet [ 2, 3 ]
	       , transitionen = t
	       , startzustand	 = 2
	       , startsymbol	 = 'A'
	       , akzeptiert	 = Leerer_Keller
	       }


-- | die Tabelle direkt aus dem Skript abgeschrieben

t = listToFM [ -- Spalte 1 (Z 2, x = '0')
	       ( ( Just 'a' , 2, 'A'), mkSet [ ( 2, "BA") ] )
	     , ( ( Just 'a' , 2, 'B'), mkSet [ ( 2, "BB"), ( 3, "") ] )
	     , ( ( Just 'a' , 2, 'C'), mkSet [ ( 2, "BC") ] )
	     
	     -- Spalte 2 (Z 2, x = '1')
	     , ( ( Just 'b' , 2, 'A'), mkSet [ ( 2, "CA") ] )
	     , ( ( Just 'b' , 2, 'B'), mkSet [ ( 2, "CB") ] )
	     , ( ( Just 'b' , 2, 'C'), mkSet [ ( 2, "CC"), (3, "") ] )
	     
	     -- Spalte 3 (Z 2, x = epsilon)
	     , ( ( Nothing, 2, 'A'), mkSet [ ( 3, "") ] )

	     -- Spalten 4 bis 6 (Z 3)
	     , ( ( Just 'a' , 3, 'B'), mkSet [ ( 3, "" ) ] )
	     , ( ( Just 'b' , 3, 'C'), mkSet [ ( 3, "" ) ] )
	     , ( ( Nothing, 3, 'A'), mkSet [ ( 3, "" ) ] )
	     ]


-- durch List Comprehensions läßt sich das kompakter schreiben:

t' = plusFM_C union speichern pruefen

speichern = listToFM 
    [ ( ( Just x, 2, y ), mkSet [ ( 2, [h, y] ) ] )
    | (x, h) <- [ ('0', 'A'), ('1', 'C') ]
    , y <- "ABC"
    ]

pruefen	  = listToFM 
    [ ( ( x, z, y ), mkSet [ ( 3, "" ) ] )
    | (x, y) <- [ ( Just '0', 'B'), ( Just '1', 'B'), ( Nothing, 'A') ]
    , z <- [ 2, 3 ]
    ]
