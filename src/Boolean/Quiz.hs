{-# LANGUAGE MultiParamTypeClasses #-}
module Boolean.Quiz where

--  $Id$

import Boolean.Op 
import Boolean.Instance

import Autolib.TES.Type
import qualified Autolib.TES.Binu as B
import qualified Autolib.TES.Enum as E
import Autolib.Choose
import Autolib.TES.Identifier
import Autolib.TES.Position

import Autolib.Util.Zufall
import Autolib.Util.Wort
import Autolib.Util.Seed
import Autolib.Set

import Autolib.ToDoc
import Autolib.Reader

import Util.Datei

import Inter.Types
import Inter.Quiz

roll :: BIC -> IO ( Term Identifier ( Op Bool ))
roll bic = do
    -- throw term
    t <- choose ( operators_in_instance bic ) ( formula_size bic )
    -- insert negation
    p <- eins $ pos t
    let s = poke t ( p, Node (read "!") [ peek t p ] )
    -- insert variable
    qs <- permutation $ leafpos s
    let qvs = zip qs 
	    $ do c <- "xxyyz" ; return $ Autolib.TES.Type.Var $ mknullary [c]
    let r = pokes s qvs
    return r

make :: Make
make = quiz Boolean 
     $ BIC { formula_size = 5
	   , operators_in_instance = B.Binu
                { B.binary  = [ read "&&", read "||", read "<", read "==" ]
		, B.unary   = [ ]
		, B.nullary = [ read "true", read "false" ]
		}
	   , operators_in_solution = mkSet $ read "[ !, ||, && ]" 
	   }

instance Generator Boolean BIC BI where
   generator p conf key = do
       x <- roll conf
       return $ BI { formula = x
		   , operators = operators_in_solution conf
		   }

instance Project Boolean BI BI where
   project p bi = bi



