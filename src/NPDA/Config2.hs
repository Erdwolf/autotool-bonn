module NPDA.Config2 where

import Machine.Acceptor.Type2

import Language.Sampler
import NPDA.Type
import NPDA.Beispiel
import NPDA.Property 

import Autolib.ToDoc
import Autolib.Reader


import Data.Typeable

example :: Type ( NPDA Char Char Int ) String Property
example = Make
	    { machine_desc = "Kellerautomat"
            , source = Language.Sampler.example
	    , properties = []
	    , cut = 30 
	    , start = anbn
	    }

-- local variables:
-- mode: haskell
-- end:
--  Imported from other files :-
