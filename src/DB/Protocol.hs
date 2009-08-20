-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module DB.Protocol where

import DB.Data

-- import Text.XML.HaXml.Types
-- import Text.XML.HaXml.Haskell2Xml


data Request_Code = Put | Get | Delete
		  | Shutdown -- speziell behandeln
		  | Discard

-- {-! for Request_Code derive : Haskell2Xml !-}

data Request = Request
	     { request_code :: Request_Code
	     , base :: String -- Name der Datenbank (erstmal ignorieren)
	     , entry :: Entry
	     }

-- {-! for Request derive : Haskell2Xml !-}

data Response = OK Entry
		   | Failure String

-- {-! for Response derive : Haskell2Xml !-}

