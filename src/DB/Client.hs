

import DB.Protocol
import DB.Config
import DB.Data
import DB.Record
import DB.Util
import DB.Action

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
				   , contents = Just
				              $ Contents 
				              $ toContents
				              $ Record { name = "Foo"
						       , vorname = "Bar"
						       , email = "foo@bar"
						       , punkte = [("A", 2)]
						       }
				   }
		   }

req2 = 
	   Request { request_code = Get
		   , base = "BK03"
		   , entry = Entry { ident = "foobar"
				   , password = "12345"
				   , contents = Nothing
				   }
		   }

main = do
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing

    OK e1 <- action "theo1" port req1
    let c1 = fmap fromCon $ contents e1 :: Maybe Record
    print c1

    OK e2 <- action "theo1" port req2
    let c2 = fmap fromCon $ contents e2 :: Maybe Record
    print c2

	  


