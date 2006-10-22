-- CGI main module

import DB.Protocol
import DB.Data
import DB.Util
import DB.Record
import DB.Action
import DB.Config

import Network.CGI
import qualified Text.XHtml
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Types
import Text.XML.HaXml.Haskell2Xml

import Maybe

main :: IO ()
main = wrapper $ \ env -> doctor $ cgi env

doctor :: IO Doc -> IO Text.XHtml.Html
doctor idoc = do
    doc <- idoc
    let html = Text.XHtml.thecode $ Text.XHtml.linesToHtml $ lines $ show doc
    return html

cgi :: [(String,String)] -> IO Doc
cgi env = do
    let idy = fromMaybe "foo" $ lookup "id" env
    let pas = fromMaybe "bar" $ lookup "pass" env
    let emi = fromMaybe "bar" $ lookup "email" env
    let act = fromMaybe "get" $ lookup "action" env

    case act of
         "get" -> handle $ 
 		  Request { request_code = Get
		   , base = "BK03"
		   , entry = Entry { ident = idy
				   , password = pas
				   , contents = Nothing
				   }
		   }
	 "put" -> handle $
 		  Request { request_code = Put
		   , base = "BK03"
		   , entry = Entry { ident = idy
				   , password = pas
				   , contents = Just
				              $ Contents 
				              $ toContents
				              $ Record { name = "Foo"
						       , vorname = "Bar"
						       , email = emi
						       }
				   }
		   }


handle :: Request -> IO Doc
handle req = do
    res <- action host port req
    
    case res of
        OK e -> do
	    let c = fmap fromCon $ contents e :: Maybe Record
	    return $ Text.PrettyPrint.HughesPJ.text $ show c
	Failure msg -> do
	    return $ Text.PrettyPrint.HughesPJ.text msg



