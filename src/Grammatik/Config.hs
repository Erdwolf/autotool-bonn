{-# language DeriveDataTypeable #-}

module Grammatik.Config where

import Language.Syntax

import qualified Grammatik.Type as G
import Grammatik.Property 

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable


data Config = 
     Config { lang :: Language.Syntax.Type
	    , max_length :: Int -- ^ Wörter höchstens so lang
	    , max_num :: Int -- ^ höchstes so viele
	    , properties :: [ Property ]
	    , cut :: Int -- ^ soviele Schritte (der Maschine)
	    , start :: G.Grammatik
	    }
     deriving ( Typeable )

{-! for Config derive: ToDoc, Reader !-}


example = Config
	    { lang = Ordered_Gleich "ab"
	    , max_length = 10 -- Wörter höchstens so lang
	    , max_num = 100 -- höchstes so viele
	    , properties = []
	    , cut = 30 -- soviele Schritte (der Maschine)
	    , start = G.example
	    }

-- local variables:
-- mode: haskell
-- end:
