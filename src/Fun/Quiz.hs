module Fun.Quiz where

-- $Id$

import Fun.Quiz.Type
import Fun.Type
import Fun.Table
import Fun.Examples
import Fun.Check
import Fun.Create
import qualified RAM.Builtin


import Inter.Types
import Challenger.Partial


import Util.Datei
import Util.Cache
import Util.Seed

import Array
import Reporter
import ToDoc

instance Partial Quiz Tafel Fun where
    initial p i   = Fun.Examples.plus
    partial p i b = do 	         
	  check_builtins RAM.Builtin.none b
	  check_arity 2 b
    total   p i b = do
          inform $ text "Die Wertetabelle Ihrer Funktion ist:"
	  let ((0,0), xy) = bounds i
          let t = tabulate b xy
	  inform $ nest 4 $ frame t
	  let diffs = do
	          xy <- indices i
		  guard $ i ! xy /= t ! xy
		  return (xy, i!xy, t!xy)
	  when ( not $ null diffs ) $ do
	       inform $ text "Die Tabellen stimmen nicht überein:"
	       reject $ nest 4 $ toDoc diffs
	  inform $ text "Die Tabellen stimmen überein."


quiz :: String -- Aufgabe
     -> String -- Version
     -> Int -- größe der funktion
     -> Int -- tabellengröße
     -> Var Quiz Tafel Fun 
quiz auf ver s t =  
         Var { problem = Quiz
	     , aufgabe = auf
	     , version = ver
	     -- erzeugt cached version der instanz (o. ä.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tatsächliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ key -> do
	           seed $ read key
	           ( f, tab ) <- cache (  Datei { pfad = [ "autotool", "cache"
					   , auf, ver
					   ]
				  , name = key ++ ".cache" 
				  }
			 ) ( nontrivial s t )
	           return $ do
	               inform $ vcat
	                  [ text "Gesucht ist eine zweistellige primitiv rekursive Funktion"
	                  , text "mit folgender Wertetabelle:"
			  , nest 4 $ frame tab
			  ]
	               return tab
	     }

