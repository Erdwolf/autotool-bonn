module PCProblem.Quiz where

-- $Id$

import PCProblem.Type
import PCProblem.Param
import PCProblem.Generator


import Inter.Types
import Challenger.Partial

import Util.Datei
import Util.Cache
import Util.Seed

import Array
import Reporter
import List (isPrefixOf)
import ToDoc

instance Partial PCProblem PCP Folge where
    initial p i   = 
        case do let PCP uvs = i
		(k, (u,v)) <- zip [1..] uvs
		guard $ isPrefixOf u v || isPrefixOf v u
		return k
	of   []     -> [ ] -- sollte nicht passieren
	     k : ks -> [k] -- so k�nnte es losgehen

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
		      [ text "Die eine mu� ein Pr�fix der anderen sein,"
		      , text "nach L�schen des gemeinsamen Pr�fixes"
		      , nest 4 $ toDoc com
		      , text "entstehen jedoch die Reste"
		      , nest 4 $ toDoc ( urest, vrest )
		      ]
          
    total   p i b = do
	  when ( null b ) $ do
	       reject $ text "Das L�sungswort darf nicht leer sein."
	  let ( us, vs ) = lr i b
          assert ( us == vs )
	         $ text "Sind die Zeichenketten gleich?"


quiz :: String -- Aufgabe
     -> String -- Version
     -> Param
     -> Var PCProblem PCP Folge
quiz auf ver par =  
         Var { problem = PCProblem
	     , aufgabe = auf
	     , version = ver
	     -- erzeugt cached version der instanz (o. �.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tats�chliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ key -> do
	           seed $ read key
	           ( p, f ) <- cache (  Datei { pfad = [ "autotool", "cache"
					   , auf, ver
					   ]
					, relativzahl = error "PCProblem.Quiz.relativzahl"
				  , name = key ++ ".cache" 
				  }
			 ) ( generator par )
	           return $ do
	               inform $ vcat
	                  [ text "L�sen Sie diese Instanz des Postschen Korrespondenz-Problems:"
			  , nest 4 $ toDoc p
			  ]
	               return p
	     }

