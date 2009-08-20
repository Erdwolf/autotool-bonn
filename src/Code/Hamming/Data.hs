-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Code.Hamming.Data 

( Goal (..)
, Attribut (..)
, Config (..)
, LR (..), Code
-- , attributes
, example
)

where

import Code.Huffman.LR

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml
import Autolib.Reader
import Autolib.ToDoc

-- | damit übernimmt der parser die prüfung darauf,
-- daß nur zwei buchstaben verwendet werden
type Code = [[ LR ]]

data Goal = Fixed 
          | Atleast
	  | Atmost 
     deriving ( Eq, Ord )

$(derives [makeReader, makeToDoc] [''Goal])
-- {-! for Goal derive : ToDoc, Reader, Haskell2Xml !-}

data Config = 
     Config { width  :: ( Goal, Int )
	    , size   :: ( Goal, Int )
	    , distance :: ( Goal, Int )
	    , optimize :: Attribut
	    }
     deriving ( Eq, Ord, Typeable )

data Attribut = Width | Size | Distance deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Attribut])
-- {-! for Attribut derive : ToDoc, Reader, Haskell2Xml !-}

attributes = [ (width, "Breite"), (size, "Größe"), (distance, "Abstand") ]

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive : ToDoc, Reader, Haskell2Xml !-}

example = Config { size = ( Atleast, 5 )
	      , width = ( Fixed,  4 )
	      , distance = ( Atleast, 2 )
	      , optimize = Size
	      }


