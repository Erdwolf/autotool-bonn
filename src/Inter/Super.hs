-- | to edit the problem configuration
-- vorteil gegenüber direktem zugriff auf datenbank:
-- syntaxprüfung der config
-- später auch beispielrechnungen

module Main where

import Inter.Make
import Challenger.Partial
import Inter.Types
import Inter.Click
import Inter.Errmsg

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader

import Random
import Network.CGI
import Data.Typeable
import Data.Maybe
import Control.Monad
import qualified Control.Exception
import Text.Html hiding ( text, input, version, width, height )

-- for testing:
import qualified HTWK.SS04.Informatik.Boolean as B

main :: IO ()
main = wrapper $ \ env ->  
         iface ( Par { makers = B.makers 
		     , maker = error "Super.maker"
		     , input = error "Super.input"
		     , height = error "Super.height"
		     , width = 60
		     } ) env
             `Control.Exception.catch` \ err -> 
                 return $ p << pre << primHtml ( show err )

data Par =
     Par { makers :: [ Make ]
	 , maker :: Make
	 , input :: String
	 , height :: Int
	 , width :: Int
	 }

iface :: Par -> [ (String, String) ] -> IO Html
iface par env = do
    
    let presenter = h3 << "all Makers and their types:" 
	    +++ p << pre << show ( present $ makers par )

    let mcs = lookup "Maker" env 
    let mks  = do 
          m @ ( Make doc fun ex ) <- makers par
          guard $ Just doc == mcs
          return m

    center <- case mks of
          []   -> return $ h3 << ( "No Maker with name: " ++ show mcs )
	  [ mk @ ( Make doc fun ex ) ] -> do  
                  let inp = fromMaybe (show ex) $ lookup "input" env
		  c <- core ( par { maker = mk 
				  , input = inp
				  , height = length $ lines inp
				  } )
		  return $  h3 << ( "Chosing Maker: " ++ show mcs ) +++ c 
          _    -> return $ h3 << ( "More than one Maker with name: " ++ show mcs )

    return $ page par [ presenter, center ]

core :: Par -> IO Html 
core par = case maker par of 
    Make doc fun ex -> do    
        let ty = primHtml "has type"
	        +++ pre << show ( toDoc $ typeOf fun )
        let scratch = textarea ( primHtml $ input par  ) 
					! [ name "input"
					  , rows $ show $ height par + 2
					  , cols $ show $ width  par
					  ]
			   +++ br

			   +++ submit "Click" ( show Submit   ) +++ " " 
			   +++ submit "Click" ( show Previous ) +++ " " 
			   +++ submit "Click" ( show Example  ) +++ " " 

			   +++ reset  "Click" ( show Reset    ) 

        va <- case parse (parse_complete reader) "input" $ input par of
		Left e -> do
		    return $ primHtml $ show $ errmsg (width par) e $ input par
                Right conf -> do
                    h <- handler ( fun conf )
	 	    return $ primHtml "gelesen:" +++ pre << show conf +++ h 
        return $ ty +++ scratch +++ va

handler :: Partial p i b 
	=> Var p i b -> IO Html
handler var = do
    let header = table << aboves 
            [ besides [ td << "Problem", td << show (problem var) ]
	    , besides [ td << "Aufgabe", td << aufgabe var ]
	    , besides [ td << "Version", td << version var ]
	    ]     
    m0 <- randomRIO (0, 999999 :: Int) -- some matrikelnummer
    let m = show m0
    k <- key var m -- some key
    g <- gen var k
    let ( Just i , com :: Html ) = export g
    let desc = describe (problem var) i
        ini = initial (problem var) i
    let belly = table << aboves 
           [ besides [ td << "Matrikel", td << show m ]
	   , besides [ td << "Key", td << show k ]
	   , besides [ td << "Instanz", td << pre << show (toDoc i) ]
	   , besides [ td << "Description", td << pre << show desc ]
	   , besides [ td << "Initial", td << pre << show ( toDoc ini ) ]
	   ]
    return $ header +++ belly

page par contents = 
    let 
	heading = h2 << "Autotool Super Interface"
	pref = preface par 
    in  header << thetitle << "Super.cgi"
            +++ body ( form ( foldr1 (+++) 
			      $ [ heading , hr , pref , hr ] ++ contents )
				   ! [ Text.Html.action "Super.cgi" , method "POST" ]
		 )
         
preface par = table << aboves [ besides ( selector ( makers par) 
					  ++ [ td << submit "Click" ( show Change ) ] ) ]
selector mks =
    let docs = map primHtml $ do Make doc _ _ <- mks ; return doc
    in  [ td << "Maker:" ,  td << menu "Maker" docs ]

