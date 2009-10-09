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
    -- further fields may be added in future protocol versions
}

$(derives [makeToTransport] [''ServerInfo])
