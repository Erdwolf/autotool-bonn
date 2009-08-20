{-# LANGUAGE TemplateHaskell #-}
module Graph.Hamilton.Config where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Config = Config 
	    { nodes   :: Int 
	    , edges   :: Int 
	    }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

example :: Config
example = Config { nodes = 10
	    , edges  = 30
	    }

-- local variables:
-- mode: haskell
-- end;