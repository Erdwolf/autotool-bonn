module Info (
    protocolVersion,
    serverName,
    serverVersion,
    language
) where

import Types.Version
import qualified Autolib.Multilingual as AM

protocolVersion :: Version
protocolVersion = Version { major = 0, minor = 0, micro = 0 }

serverName :: String
serverName = "autotool"

serverVersion :: Version
serverVersion = Version { major = 0, minor = 0, micro = 1 }

language :: AM.Language
language = AM.DE -- UK, NL
