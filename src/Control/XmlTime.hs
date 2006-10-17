{-# LINE 1 "Control/XmlTime.hs.drift" #-}
-- -*- mode: haskell -*-

module Control.XmlTime where

import Text.XML.HaXml.Haskell2Xml

data XmlTime = XmlTime String

{-! for XmlTime derive : Haskell2Xml !-}



instance Haskell2Xml XmlTime where
    toHType v =
        Defined "XmlTime" [] [Constr "XmlTime" [] [toHType aa]]
      where
        (XmlTime aa) = v
    fromContents (CElem (Elem constr [] cs):etc)
        | "XmlTime" `isPrefixOf` constr =
            (\(aa,_)-> (XmlTime aa, etc)) (fromContents cs)
    toContents v@(XmlTime aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]

--  Imported from other files :-
