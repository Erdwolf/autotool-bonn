{-# LINE 1 "Inter/ID.hs.drift" #-}
-- -*- mode: haskell -*-

module Inter.ID where

import Reader
import ToDoc

data ID = ID
          { matrikel :: String
          , passwort :: String
          , problem  :: String
          , aufgabe  :: String
          , version  :: String
          }

{-! for ID derive : Reader, ToDoc !-}

instance Show ID where
    show = render . toDoc

instance Read ID where
    readsPrec = parsec_readsPrec

instance Reader ID where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
                      ((do guard (d < 9)
                           my_reserved "ID"
                           my_braces ((do my_reserved "matrikel"
                                          my_equals
                                          aa <- readerPrec 0
                                          my_comma
                                          my_reserved "passwort"
                                          my_equals
                                          ab <- readerPrec 0
                                          my_comma
                                          my_reserved "problem"
                                          my_equals
                                          ac <- readerPrec 0
                                          my_comma
                                          my_reserved "aufgabe"
                                          my_equals
                                          ad <- readerPrec 0
                                          my_comma
                                          my_reserved "version"
                                          my_equals
                                          ae <- readerPrec 0
                                          return (ID aa ab ac ad ae)))))

instance ToDoc ID where
    toDocPrec d (ID aa ab ac ad ae) = docParen (d >= 10)
              (text "ID" </> dutch_record
               [text "matrikel" <+> equals <+> toDocPrec 0 aa,
                text "passwort" <+> equals <+> toDocPrec 0 ab,
                text "problem" <+> equals <+> toDocPrec 0 ac,
                text "aufgabe" <+> equals <+> toDocPrec 0 ad,
                text "version" <+> equals <+> toDocPrec 0 ae])

--  Imported from other files :-