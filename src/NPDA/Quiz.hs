module NPDA.Quiz where

--  $Id$

import NPDA.Type
import Language.Type

import qualified Grammatik.CF.Language as L
import qualified Grammatik.CF.Generate as G
import qualified NPDA.Inter as I
import qualified Machine.Acceptor.Type as A

import Util.Cache
import Util.Datei
import Util.Seed
import Informed
import ToDoc
import Inter.Types

make :: ( G.Config , I.Config )
     -> Var A.Acceptor 
	    ( A.Type ( NPDA Char Char Int ) String  )  
	    ( NPDA Char Char Int )
make ( gconf, iconf ) = 
    let auf = "NPDA"
	ver = "Quiz"
    in Var { problem = A.Acceptor
	, aufgabe = auf
	, version = ver
	, key = \ matrikel -> return matrikel
	, gen = \ key -> do
	    seed $ read key
	    g <- cache (  Datei { pfad = [ "autotool", "cache" , auf, ver ]
                                     , name = key
                                     , extension = "cache"
                                     }
                            ) ( G.throw $ gconf )
	    let l = L.make "Quiz" g
		m = I.max_num iconf
		e = I.max_length iconf
	        small w = length w <= e
	    yeah <- samples      l m 0
	    noh  <- anti_samples l m 0
	    return $ return $ A.Make
		   { A.machine_info = text "Keller-Automat"
		   , A.data_info = info $ l
		   , A.yeah = take m $ filter small yeah
		   , A.noh  = take m $ filter small noh
		   , A.cut  = I.cut iconf
		   , A.check = I.check iconf
		   , A.start = I.start iconf
		   }

	}
