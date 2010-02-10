module Inter.Quiz where


import Challenger.Partial
import Inter.Types
import Control.Types

import Util.Datei
import Autolib.Util.Seed
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash

import Data.Maybe
import Data.Char
import Data.Typeable

-- | generator könnte noch zusätzliche information erzeugen
-- (und in cache schreiben) bsp. zu PCP-instanz auch die lösung
-- mit project holt man sich die instanz
class ( Reader k, ToDoc k ) => Generator p conf k | p conf -> k where
      generator :: p -> conf -> String -> IO k

class Project p k i | p k -> i where
      project :: p -> k -> i


make :: ( Generator p conf k, Project p k i , ToDoc conf
	, Partial p i b 
	, V p i b
	)
     => p
     -> conf
     -> Var p i b
make ( p :: p ) ( conf :: conf ) = this 
  where this = Var 
             { problem = p
	     , tag = dashed p ++ "-" ++ "Quiz"
	     -- erzeugt cached version der instanz (o. ä.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return $ mat
	     -- holt tatsächliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ vnr manr key cache -> do

             --  generate this $ fromIntegral $ hash ( vnr, manr, key )

                   seed $ fromIntegral $ hash $ show ( toDoc conf ) ++ key
                   k <- cache 
	               (  Datei { pfad = [ "autotool", "cache"
			   , toString vnr
			   , fromMaybe (dashed p) $ fmap toString manr
			     ]
		          , name = key
			  , extension = "cache" 
  		          }
       	                ) ( generator p conf key )
	           return $ return $ project p k

             , generate = \ salt cache -> do
                   seed $ salt 
                   k <- cache 
	               (  Datei { pfad = [ "autotool", "new-cache" ]
		          , name = show salt
			  , extension = "cache" 
  		          }
       	                ) ( generator p conf $ show salt )
	           return $ return $ project p k

	     }

quiz :: ( Generator p conf k , Project p k i
	,  Partial p i b 
	, V p i b
	, Verify p conf
	, Typeable conf, Reader conf, ToDoc conf
	, Show conf, Read conf
	)
     => p -- ^ problem type
     -> conf -- ^ default configuration
     -> Make
quiz ( p :: p ) ( conf0 :: conf ) = Make p
    ( dashed p ++ "-Quiz" )
    ( \ conf -> make p conf )
    ( verify p )
    conf0

