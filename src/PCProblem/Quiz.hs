module PCProblem.Quiz where

-- -- $Id$

import PCProblem.Type
import PCProblem.Param
import PCProblem.Generator


import Inter.Types
import Challenger.Partial

import Util.Datei
import Util.Cache
import Autolib.Util.Seed

import Data.Array
import Autolib.Reporter
import Data.List (isPrefixOf)
import Autolib.ToDoc
import Autolib.Informed


instance Partial PCProblem PCP Folge where

    describe p i = 
        vcat [ text "L�sen Sie diese Instanz des Postschen Korrespondenz-Problems:"
	     , nest 4 $ toDoc i
	     ]



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


make :: String -- Aufgabe
     -> String -- Version
     -> ( Key -> IO PCP )
     -> Var PCProblem PCP Folge
make auf ver gene =  
         Var { problem = PCProblem
	     , aufgabe = auf
	     , version = ver
	     -- erzeugt cached version der instanz (o. �.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tats�chliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ key -> do
                   p <- cache 
	               (  Datei { pfad = [ "autotool", "cache"
			   , auf, ver
			     ]
		          , name = key
			  , extension = "cache" 
  		          }
       	                ) ( gene key )
	           return $ do
	               return p
	     }

quiz :: Param -> Key -> IO PCP
quiz par = \ key -> do
     seed $ read key
     (p, f) <- generator par
     return p

make_quiz :: Make
make_quiz = Make "PCP-Quiz"
    ( \ par -> make "PCP" "Quiz" $ quiz par )
    PCProblem.Param.g

fixed :: PCP -> Key -> IO PCP
fixed pcp = \ key -> return pcp

make_fixed :: Make
make_fixed = Make "PCP-Fixed"
    ( \ pcp -> make "PCP" "Fixed" $ fixed pcp )
    ( PCP [ ("bba","b"),("a","b"),("b","ab") ] )



