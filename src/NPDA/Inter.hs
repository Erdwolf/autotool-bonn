module NPDA.Inter where

-- $Id$

import NPDA.Type
import qualified Machine.Acceptor.Type as A
import qualified Machine.Acceptor.Inter

import Language.Type

import Reporter
import qualified Reporter.Checker as C
import ToDoc
import Informed
import Inter.Types

data Config = 
     Config { lang :: Language
	    , max_length :: Int -- Wörter höchstens so lang
	    , max_num :: Int -- höchstes so viele
	    , check :: C.Type ( NPDA Char Char Int ) -- properties
	    , cut :: Int -- soviele Schritte (der Maschine)
	    , start :: NPDA Char Char Int
	    }

make :: Config 
     -> Var A.Acceptor 
	    ( A.Type ( NPDA Char Char Int ) String  )  
	    ( NPDA Char Char Int )
make config = 
    Var { problem = A.Acceptor
	, aufgabe = "NPDA" ++ C.nametag ( check config )
	, version = nametag $ lang config
	, key = \ matrikel -> return matrikel
	, gen = \ key -> do
	    let l = lang config
		m = max_num config
		e = max_length config
	    yeah <- samples      l m e
	    noh  <- anti_samples l m e
	    return $ return $ A.Make
		   { A.machine_info = text "Keller-Automat"
		   , A.data_info = info $ lang config
		   , A.yeah = yeah
		   , A.noh = noh
		   , A.cut = cut config 
		   , A.check = check config
		   , A.start = start config
		   }
	}
