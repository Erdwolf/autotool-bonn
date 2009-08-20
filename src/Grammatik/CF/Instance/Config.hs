-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Grammatik.CF.Instance.Config 

( Config (..)
-- , Long (..)
, module Autolib.Long
)

where

-- -- $Id$

import Language.Syntax
import Grammatik.Property

import Autolib.Long
import Autolib.ToDoc
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml

import Data.Typeable

data Config =
     Config { lang :: Language.Syntax.Type
	    , properties :: [ Property ]
	    , yeah :: Long ( Long Char )
	    , noh :: Long ( Long Char )
	    }
     deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: ToDoc, Reader, Haskell2Xml !-}

