-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Grammatik.CF.Problem.Config where

-- -- $Id$

import Language.Syntax
import Grammatik.Property

import Autolib.ToDoc
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml

import Data.Typeable

data Config =
     Config { lang :: Language.Syntax.Type
	    , properties :: [ Property ]
	    , num_samples :: Int -- ^ anzahl der samples
	    , min_sample_length :: Int -- ^ minimale länge der samples
	    , max_sample_length :: Int -- ^ maximal länge der samples
	    }
     deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: ToDoc, Reader, Haskell2Xml !-}

example :: Config
example = Config
	{ lang = Lukas
        , properties = [ Kontextfrei ]
	, num_samples = 50
	, min_sample_length = 4
	, max_sample_length = 40
	}
