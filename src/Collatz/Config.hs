-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Collatz.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	     { min_start :: Integer
	     , max_start :: Integer
	     , min_length :: Integer
	     , max_length :: Integer
             , min_top :: Integer
             , max_top :: Integer
	     }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

rc :: Config
rc = Config { min_start = 10 ^ 3
	    , max_start = 10 ^ 5
	    , min_length = 10 ^ 2
	    , max_length = 10 ^ 5
	, min_top = 2 ^ 2
	, max_top = 2 ^ 20
	}

