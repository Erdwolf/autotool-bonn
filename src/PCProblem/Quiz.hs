module PCProblem.Quiz where

-- -- $Id$

import PCProblem.Type
import PCProblem.Param
import PCProblem.Generator


import Inter.Types
import Inter.Quiz
import Challenger.Partial

import Data.Array
import Autolib.Reporter
import Data.List (isPrefixOf)
import Autolib.ToDoc
import Autolib.Informed


instance Partial PCProblem PCP Folge where

    describe p i = 
        vcat [ text "Lösen Sie diese Instanz des Postschen Korrespondenz-Problems:"
	     , nest 4 $ toDoc i
	     ]



    initial p i   = 
        case do let PCP uvs = i
		(k, (u,v)) <- zip [1..] uvs
		guard $ isPrefixOf u v || isPrefixOf v u
		return k
	of   []     -> [ ] -- sollte nicht passieren
	     k : ks -> [k] -- so könnte es losgehen

    partial p i @ ( PCP uvs ) b = do 	     
          let n = fromIntegral $ length uvs
          let wrong = do k <- b ; guard $ not $ 1 <= k && k <= n ; return k
	  when ( not $ null wrong ) $ do
	       inform $ text "Diese Indizes sind nicht erlaubt:"
	       reject $ nest 4 $ toDoc wrong

	  let ( us, vs ) = lr i b
          inform $ vcat
		      [ text "Aus Ihrer Folge entstehen die Zeichenketten:"
		      , toDoc us, toDoc vs
		      ]
	  let com = common us vs
	      urest = drop (length com) us
	      vrest = drop (length com) vs
	  when ( not (null urest) && not ( null vrest )) $ do
	       reject $ vcat
		      [ text "Die eine muß ein Präfix der anderen sein,"
		      , text "nach Löschen des gemeinsamen Präfixes"
		      , nest 4 $ toDoc com
		      , text "entstehen jedoch die Reste"
		      , nest 4 $ toDoc ( urest, vrest )
		      ]
          
    total   p i b = do
	  when ( null b ) $ do
	       reject $ text "Das Lösungswort darf nicht leer sein."
	  let ( us, vs ) = lr i b
          assert ( us == vs )
	         $ text "Sind die Zeichenketten gleich?"

--------------------------------------------------------------------------


make_quiz :: Make
make_quiz = quiz PCProblem
    PCProblem.Param.g

make_fixed :: Make
make_fixed = direct PCProblem 
    ( PCP [ ("bba","b"),("a","b"),("b","ab") ] )



