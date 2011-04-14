{-# LANGUAGE TemplateHaskell #-}

module Types.ServerInfo (
    ServerInfo (..),
    module Types.Version
) where

import Types.Version

import Data.Autolib.Transport

-- the server information record
data ServerInfo = ServerInfo {
    protocol_version :: Version,
    server_name      :: String,
    server_version   :: Version
} deriving (Eq, Read, Show)

$(derives [makeToTransport] [''ServerInfo])
