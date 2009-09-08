{-# OPTIONS -fglasgow-exts #-}

{-# LANGUAGE TemplateHaskell #-}
module Number.Base.Param where

--   $Id$

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data Quelle = Bereich { von :: Integer, bis :: Integer }
            | Matrikel
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Quelle])
-- {-! for Quelle derive: ToDoc, Reader, Haskell2Xml !-}


data Param = 
     Param { quelle :: Quelle
	   , von_basis :: Int
	   , nach_basis :: Int
	   }
     deriving ( Typeable )

p :: Param
p = Param { quelle = Matrikel
	  , von_basis = 3
          , nach_basis = 5
	  }

$(derives [makeReader, makeToDoc] [''Param])
-- {-! for Param derive: ToDoc, Reader, Haskell2Xml !-}


-- local variables:
-- mode: haskell
-- end;

