module Term.Dot 

( display 
)

where

--   $Id$

import Term.Type

import Dot.Dot
import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge

instance ( Show a ) => ToDot ( Term a ) where
    toDotProgram t = "dot"
    toDotOptions t = "" -- d.h. kein rankdir
    toDot t = 
	let -- compact show, geht nur für einstellige kind-nummern
	    sp ps = "R" ++ concat ( map show ps ) 
	
            tricky cs =
                if take 1 cs `elem` [ "\"", "'" ]
                   -- dann ist es Show String|Char
                then tail ( init cs )     -- und eine "-klammer kann weg  
		else cs
            ns = do p <- positions t
                    return $ Dot.Node.blank
                           { Dot.Node.ident = sp p
                           , Dot.Node.label = 
			          Just $ tricky $ show $ symbol $ peek t p
                           , Dot.Node.shape = Nothing
                           }

            es = do p <- positions t
		    i <- [ 0 .. length (children $ peek t p) - 1 ]
		    let q = p ++ [i]
                    return $ Dot.Edge.blank
                           { Dot.Edge.from  = sp p
                           , Dot.Edge.to    = sp q
                           , Dot.Edge.directed = True
                           }
         in  Dot.Graph.Type
            { Dot.Graph.directed = True
            , Dot.Graph.name = "foo"
            , Dot.Graph.nodes = ns
            , Dot.Graph.edges = es
            }
  