module DB.Action where

import DB.Protocol
import DB.Util

import Network
import IO

action :: String -> Int -> Request -> IO Response
action host port req = do
    putStrLn $ "connectTo " ++ show (host, port) ; hFlush stdout

    h <- connectTo host ( PortNumber $ fromIntegral port )
    hWriteXmlUtil h req 

    res <- hReadXmlUtil h 
    hClose h

    return res
