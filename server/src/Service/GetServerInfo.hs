module Service.GetServerInfo (
    get_server_info
) where

import Types.TT
import Types.ServerInfo
import Info

get_server_info :: IO (TT ServerInfo)
get_server_info = return $ TT $ ServerInfo {
    protocol_version = protocolVersion,
    server_name = serverName,
    server_version = serverVersion
 }
