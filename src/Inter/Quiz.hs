module Inter.Quiz where

--  $Id$

import Challenger.Partial
import Inter.Types
import Control.Types

import Util.Datei
import Util.Cache
import Autolib.Util.Seed
import Autolib.ToDoc
import Autolib.Reader

import Data.Maybe
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

-- | generator könnte noch zusätzliche information erzeugen
-- (und in cache schreiben) bsp. zu PCP-instanz auch die lösung
-- mit project holt man sich die instanz
class ( Read k, ToDoc k ) => Generator p conf k | p conf -> k where
      generator :: p -> conf -> String -> IO k

class Project p k i | p k -> i where
      project :: p -> k -> i

make :: ( Generator p conf k, Project p k i , Partial p i b 
	, V p i b
	)
     => p
     -> conf
     -> Var p i b
make ( p :: p ) ( conf :: conf ) =  
         Var { problem = p
	     , tag = show (toDoc p) ++ "-" ++ "Quiz"
	     -- erzeugt cached version der instanz (o. ä.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tatsächliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ vnr manr key -> do
                   seed $ read key
                   k <- cache 
	               (  Datei { pfad = [ "autotool", "cache"
			   , toString vnr
			   , fromMaybe (show $ toDoc p) $ fmap toString manr
			     ]
		          , name = key
			  , extension = "cache" 
  		          }
       	                ) ( generator p conf key )
	           return $ return $ project p k
	     }

quiz :: ( Generator p conf k , Project p k i,  Partial p i b 
	, V p i b
	, Typeable conf, Reader conf, ToDoc conf
	-- , Haskell2Xml conf
	, Show conf, Read conf
	)
     => p -- ^ problem type
     -> conf -- ^ default configuration
     -> Make
quiz ( p :: p ) ( conf0 :: conf ) = Make 
    ( show (toDoc p) ++ "-Quiz" )
    ( \ conf -> make p conf )
    conf0

