
import DB.Data
import DB.Protocol
import DB.Config
import DB.Util

import Data.FiniteMap

import Text.XML.HaXml.Haskell2Xml
import Text.XML.HaXml.Pretty
import Text.PrettyPrint.HughesPJ


import System
import Network
import qualified Posix
import IO
import Exception

main :: IO ()
main = do
    -- TODO : mappe von file lesen
    server port emptyFM
    -- TODO : mappe zurückschreiben
    return ()

-- single-threaded server: alles schön der reihe nach

server :: Int -> Map -> IO Map
server portnum m = do
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing
    let port = PortNumber $ fromIntegral portnum
    s <- listenOn port
    hPutStrLn stderr $ "started DB server on port " ++ show portnum
    handler s m

handler :: Socket -> Map -> IO Map
handler s m = do
    ( h, rhost, rport ) <- accept s
    hPutStrLn  stderr $ "accepting connection from " ++ show (rhost, rport)

    req <- hReadXmlUtil h -- TODO : fehlerbehandlung

    -- hPutStrLn stderr $ render ( text "parsed input:" $$ document ( toXml req ))


    -- TODO: passwort-abfragen

    let e = entry req
    case request_code req of
         Discard -> do 
		    hClose h
                    handler s m
         Put -> do
		    let res = OK e
		    hWriteXmlUtil h res
		    hClose h
		    handler s $ addToFM m (ident e) e
	 Get -> do
	            case lookupFM m (ident e) of
		        Nothing -> do
			    hWriteXmlUtil h $ Failure "not in database"
			    hClose h
			    handler s $ m
			Just ee -> do
			    let res = OK e
			    hWriteXmlUtil h res
			    hClose h
			    handler s m
	 Shutdown -> do
	      hClose h
	      return m


