module NPDA.Inter where

--   $Id$

import NPDA.Type
import NPDA.Beispiel
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

example l = Config
	    { lang = l
	    , max_length = 10 -- Wörter höchstens so lang
	    , max_num = 100 -- höchstes so viele
	    , check = C.wahr
	    , cut = 30 -- soviele Schritte (der Maschine)
	    , start = anbn
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
	        small w = length w <= e
	    yeah <- samples      l m 0
	    noh  <- anti_samples l m 0
	    return $ return $ A.Make
		   { A.machine_info = text "Keller-Automat"
		   , A.data_info = info $ lang config
		   , A.yeah = take m $ filter small yeah
		   , A.noh  = take m $ filter small noh
		   , A.cut  = cut config 
		   , A.check = check config
		   , A.start = start config
		   }
	}
