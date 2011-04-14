-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Turing.Config where

import Language.Syntax

import Turing.Type
import Turing.Example
import Turing.Property 

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable


data Config = 
     Config { lang :: Language.Syntax.Type
	    , max_length :: Int -- ^ Wörter höchstens so lang
	    , max_num :: Int -- ^ höchstes so viele
	    , properties :: [ Property ]
	    , cut :: Int -- ^ soviele Schritte (der Maschine)
	    , start :: Turing Char Int
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])



example = Config
	    { lang = Ordered_Gleich "ab"
	    , max_length = 10 -- Wörter höchstens so lang
	    , max_num = 100 -- höchstes so viele
	    , properties = []
	    , cut = 30 -- soviele Schritte (der Maschine)
	    , start = Turing.Example.student
	    }

