-- meta information about autotool server

module Info (
    protocolVersion,
    serverName,
    serverVersion,
    language
) where

import Types.Version
import qualified Autolib.Multilingual as AM

protocolVersion :: Version
protocolVersion = Version { major = 0, minor = 1, micro = 0 }

serverName :: String
serverName = "autotool"

serverVersion :: Version
serverVersion = Version { major = 0, minor = 2, micro = 0 }

language :: AM.Language
language = AM.DE -- UK, NL
