module Boolean.Quiz where

--  $Id$

import Boolean.Op 
import Boolean.Data
import Boolean.Instance

import Autolib.TES.Type
import qualified Autolib.TES.Enum as E
import Autolib.TES.Identifier
import Autolib.TES.Position

import Autolib.Util.Zufall
import Autolib.Util.Wort
import Autolib.Util.Seed
import Autolib.Set

import Util.Datei
import Util.Cache

import Inter.Types

conf :: E.Binu Op
conf = E.Binu
     { E.binary  = [ read "&&", read "||", read "<", read "==" ]
     , E.unary   = [ ]
     , E.nullary = [ read "true", read "false" ]
     }

roll :: Int -> IO ( Term Identifier Op )
roll i = do
    -- throw term
    t <- E.choose conf i
    -- insert negation
    p <- eins $ pos t
    let s = poke t ( p, Node (read "!") [ peek t p ] )
    -- insert variable
    qs <- permutation $ leafpos s
    let qvs = zip qs 
	    $ do c <- "xxyyz" ; return $ Autolib.TES.Type.Var $ mknullary [c]
    let r = pokes s qvs
    return r

make :: Int -> IO Inter.Types.Variant
make p = return 
       $ Inter.Types.Variant
       $ quiz "Boolean" "Quiz" p

quiz :: String -- Aufgabe
     -> String -- Version
     -> Int
     -> Inter.Types.Var Boolean BI Exp
quiz auf ver par =
    Inter.Types.Var { problem = Boolean
             , aufgabe = auf
             , version = ver
             , key = \ mat -> return mat
             , gen = \ key -> do
                   seed $ read key
                   x <- cache (  Datei { pfad = [ "autotool", "cache"
                                           , auf, ver
                                           ]
                                  , Util.Datei.name = key
                                  , extension = "cache"
                                  }
                         ) ( roll par )
                   return $ return 
			  $ BI { tag = "Quiz"
			       , formula = x
		      , operators = read "mkSet [ !, ||, && ]" 
			       }
	      }

