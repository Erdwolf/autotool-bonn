module Baum.Quiz where

--  $Id$

import Baum.Type
import Baum.Traverse
import Baum.Roll
import Baum.Reconstruct

import Inter.Types
import Util.Seed
import Util.Cache
import Util.Datei

data Config = 
     Config { nodes :: Int
	    , orders :: [ Order ]
	    }

hash :: Config -> Int
hash conf = foldl ( \ x y -> 13 * x + y + 29 ) 0
	  $ map fromEnum
	  $ show $ orders conf
     
quiz :: Config
     -> Var Reconstruct ( Traversals Identifier ) ( Baum )
quiz conf = 
    let auf = "Baum"
	ver = concat $ map show $ orders conf
    in  Var { problem = Reconstruct
        , aufgabe = auf
        , version = ver
        , key = \ matrikel -> return matrikel
        , gen = \ key -> do
          seed $ read key + hash conf
          t <- cache (  Datei { pfad = [ "autotool", "cache" , auf, ver ]
                                     , name = key
                                     , extension = "cache"
                                     }
                            ) ( roll $ nodes conf )
          let ocs = do 
		 o <- orders conf
		 return ( o , traverse o t )
          return $ return ocs
        }

generates :: [ IO Variant ]
generates = do
    os <- [  [ Pre, Post ], [ Pre, In ], [ Post, Level ], [ In, Level ] ]
    return $ return $ Variant 
	   $ quiz   $ Config { nodes = 13 , orders = os }
    