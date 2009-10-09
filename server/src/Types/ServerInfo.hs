{-# LANGUAGE TemplateHaskell #-}

module Types.ServerInfo (
    ServerInfo (..),
    module Types.Version
) where

import Types.Version

import Data.Autolib.Transport

data ServerInfo = ServerInfo {
    protocol_version :: Version,
    server_name      :: String,
    server_version   :: Version
}

$(derives [makeToTransport] [''ServerInfo])
