-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module DB.Record where

-- import Text.XML.HaXml.Haskell2Xml

data Record = Record { name :: String
		     , vorname :: String
		     , email :: String
		     , punkte :: [ (String, Int) ] -- Aufgabe, Anzahl
		     }
    deriving Show

-- {-! for Record derive : Haskell2Xml !-}

