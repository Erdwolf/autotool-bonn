{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

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
import Autolib.Hash

import Data.Maybe
import Data.Char
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

-- | generator k�nnte noch zus�tzliche information erzeugen
-- (und in cache schreiben) bsp. zu PCP-instanz auch die l�sung
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
make ( p :: p ) ( conf :: conf ) = this 
  where this = Var 
             { problem = p
	     , tag = dashed p ++ "-" ++ "Quiz"
	     -- erzeugt cached version der instanz (o. �.)
	     -- key :: Matrikel -> IO Key
	     , key = \ mat -> return mat
	     -- holt tats�chliche instanz
	     -- gen :: Key -> IO ( Reporter i )
	     , gen = \ vnr manr key -> do
#define NEW_STYLE 1
#if(NEW_STYLE)
                   generate this $ fromIntegral $ hash ( vnr, manr, key )
#else
                   seed $ read key
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
#endif
             , generate = \ salt -> do
                   seed $ salt 
                   k <- cache 
	               (  Datei { pfad = [ "autotool", "new-cache" ]
		          , name = show salt
			  , extension = "cache" 
  		          }
       	                ) ( generator p conf $ show salt )
	           return $ return $ project p k

	     }

quiz :: ( Generator p conf k , Project p k i,  Partial p i b 
	, V p i b
	, Verify p conf
	, Typeable conf, Reader conf, ToDoc conf
	-- , Haskell2Xml conf
	, Show conf, Read conf
	)
     => p -- ^ problem type
     -> conf -- ^ default configuration
     -> Make
quiz ( p :: p ) ( conf0 :: conf ) = Make 
    ( dashed p ++ "-Quiz" )
    ( \ conf -> make p conf )
    ( verify p )
    conf0

