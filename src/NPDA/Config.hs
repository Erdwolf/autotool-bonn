-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module NPDA.Config where

import Language.Syntax
import NPDA.Type
import NPDA.Beispiel
import NPDA.Property 

import Autolib.ToDoc
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml

import Data.Typeable


data Config = 
     Config { lang :: Language.Syntax.Type
	    , max_length :: Int -- ^ Wörter höchstens so lang
	    , max_num :: Int -- ^ höchstes so viele
	    , properties :: [ Property ]
	    , cut :: Int -- ^ soviele Schritte (der Maschine)
	    , start :: NPDA Char Char Int
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: ToDoc, Reader !-}



example = Config
	    { lang = Ordered_Gleich "ab"
	    , max_length = 10 -- Wörter höchstens so lang
	    , max_num = 100 -- höchstes so viele
	    , properties = []
	    , cut = 30 -- soviele Schritte (der Maschine)
	    , start = anbn
	    }

