module Turing.Check where

-- $Id$

import Turing.Type


check :: TUM y z
      => Turing y z -> Either String String
check m = case 

      [ "das Eingabe-Alphabet ist leer" 
      | isEmptySet (eingabealphabet m) 
      ]
      ++ 
      [ "das Leerzeichen geh�rt zum Eingabealphabet "	
      | leerzeichen m `elementOf` eingabealphabet m
      ]
      ++ 
      [ "das Leerzeichen geh�rt nicht zum Arbeitsalphabet "	
      | not ( leerzeichen m `elementOf` arbeitsalphabet m)
      ]
      ++
      [ "das Eingabealphabet " ++ show e 
	++ " ist keine Teilmenge des Arbeitsalphabets " ++ show a
      | let e = eingabealphabet m
      , let a = arbeitsalphabet m
      , not (isEmptySet (e `minusSet` a))
      ]
      ++
      [ "Fehler in der Programmregel " ++ show r ++ ": " ++ msg
      | r @ ((y,z),yzbs) <- fmToList $ tafel m
      , (y',z',b) <- setToList yzbs
      , msg <- [ show t ++ " geh�rt nicht zum Arbeitsalphabet" 
	       | t <- [ y, y' ] 
	       , not ( t `elementOf` arbeitsalphabet m )
	       ] 
	       ++ 
	       [ show t ++ " geh�rt nicht zur Zustandsmenge" 
	       | t <- [ z, z' ]
	       , not ( t `elementOf` zustandsmenge m )
	       ]
      ]
      ++ 
      [ "der Startzustand " ++ show z ++ " geh�rt nicht zur Zustandsmenge"
      | let z = startzustand m
      , not (z `elementOf` zustandsmenge m)
      ]
      ++ 
      [ "der Endzustand " ++ show z ++ " geh�rt nicht zur Zustandsmenge"
      | z <- setToList $ endzustandsmenge m
      , not (z `elementOf` zustandsmenge m)
      ]

  of  []  -> Right "Das ist wirklich eine nichtdeterministische Turingmaschine."
      msg -> Left $ unlines 
	  $ [ "Das ist keine nichtdeterministische Turingmaschine." ]
	    ++ msg

deterministisch :: TUM y z
      => Turing y z -> Either String String
deterministisch m = 
    let mehr = do
	    r @ ( yz, s ) <- fmToList $ tafel m
	    guard $ cardinality s > 1
	    return r
    in	case mehr of
	    [] -> Right "diese Maschinen ist deterministisch"
	    rs -> Left  $ unlines 
			( "diese Regeln sind nicht deterministisch:"
			: map show rs )

