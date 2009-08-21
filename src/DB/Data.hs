-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module DB.Data where

-- das wird serverseitig benutzt
-- der server weiß nichts über den inhalt der XML-dokumente

import Data.FiniteMap

-- import Text.XML.HaXml.Types
-- import Text.XML.HaXml.Haskell2Xml

data Contents  = Contents { unContents :: [ Content ] }

-- das können wir nicht deriven
instance Haskell2Xml Contents where
    toHType _ = Prim "Xml" "xml"
    toContents ( Contents es ) 
	= [CElem (Elem "xml" [] es )]
    fromContents ( CElem (Elem "xml" [] es ) : cs ) 
	= ( Contents es , cs )
    fromContents ( _ : cs ) = fromContents cs

data Entry = Entry { ident :: String
		   , password :: String
		   , contents :: Maybe Contents
		   }

-- {-! for Entry derive : Haskell2Xml !-}

type Map = FiniteMap String Entry

