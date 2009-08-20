-- -*- mode: haskell -*-

{-# LANGUAGE TemplateHaskell #-}
module Sortier.Netz.Xml where

-- import Text.XML.HaXml.Haskell2Xml

data XmlNetz = XmlNetz [(Int, Int)]

-- {-! for XmlNetz derive : Haskell2Xml !-}

