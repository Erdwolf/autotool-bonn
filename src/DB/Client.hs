

import DB.Protocol
import DB.Config
import DB.Data
import DB.Record
import DB.Util

import Text.XML.HaXml.Haskell2Xml
import Text.XML.HaXml.Pretty
import Text.PrettyPrint.HughesPJ


import Network
import qualified Posix
import IO


req1 = 
	   Request { request_code = Put
		   , base = "BK03"
		   , entry = Entry { ident = "foobar"
				   , password = "12345"
				   , contents = Contents 
				              $ toContents
				              $ Record { name = "Foo"
						       , vorname = "Bar"
						       , punkte = 42
						       }
				   }
		   }

req2 = 
	   Request { request_code = Get
		   , base = "BK03"
		   , entry = Entry { ident = "foobar"
				   , password = "12345"
				   , contents = Contents
				              $ toContents ()
				   }
		   }

main = do
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing

    res1 <- action "theo1" port req1
    hPutStrLn stderr $ render ( text "answer:"
        $$ document ( toXml res1 ))


    res2 <- action "theo1" port req2
    hPutStrLn stderr $ render ( text "answer:"
        $$ document ( toXml res2 ))


action :: String -> Int -> Request -> IO Response
action host port req = do
    putStrLn $ "connectTo " ++ show (host, port) ; hFlush stdout

    h <- connectTo host ( PortNumber $ fromIntegral port )
    hWriteXmlUtil h req 

    res <- hReadXmlUtil h 
    hClose h

    return res


