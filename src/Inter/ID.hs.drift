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

