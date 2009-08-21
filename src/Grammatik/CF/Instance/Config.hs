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

import Data.Typeable

data Config =
     Config { lang :: Language.Syntax.Type
	    , properties :: [ Property ]
	    , yeah :: Long ( Long Char )
	    , noh :: Long ( Long Char )
	    }
     deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Config])

