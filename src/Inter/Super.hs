-- | to edit the problem configuration
-- and to check a sample solution

--  $Id$

module Main where

import qualified Inter.Collector

import Inter.Env
import Challenger.Partial
import Inter.Types
import Inter.Click
import Inter.Area
import Inter.Make 
import Inter.Errmsg
import Inter.Evaluate

import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader

import Random
import Network.CGI
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad
import qualified Control.Exception
import Text.Html hiding ( text, input, version, width, height )


data Par =
     Par { env :: Env
	 , makers :: [ Make ]
	 }

click :: String
click = "Click"

maker :: String
maker = "Maker"

main :: IO ()
main = wrapper $ \ e ->  
         iface ( Par { makers = Inter.Collector.makers
		     -- sort of a bit of a hack:
		     , env = case lookup click e of
		          Nothing -> e
		          Just "Change" -> filter ( \ (i,v)-> i==maker ) e
		     } ) 
             `Control.Exception.catch` \ err -> 
                 return $ p << pre << primHtml ( show err )


iface :: Par -> IO Html
iface par = do
    
    let presenter = h3 << "all Makers and their types:" 
	    +++ p << pre << show ( present $ makers par )

    let mcs = lookup "Maker" $ env par
    let mks  = do 
          m @ ( Make doc fun ex ) <- makers par
          guard $ Just doc == mcs
          return m

    center <- case mks of
          []   -> return $ h3 << ( "No Maker with name: " ++ show mcs )
	  [ mk @ ( Make doc fun ex ) ] -> do  
                  let inp = fromMaybe (show ex) $ lookup "input" $ env par
		  c <- editor "config" ex (env par) $ \ conf ->
		          core ( fun conf ) ( env par )
		  return $  h3 << ( "Chosing Maker: " ++ show mcs ) +++ c 
          _    -> return $ h3 << ( "More than one Maker with name: " ++ show mcs )

    return $ page par [ presenter, center ]

core ::  ( Reader b, Partial p i b )
     => Var p i b
     -> Env
     -> IO Html 
core ( var :: Var p i b ) env = do
    let p = problem var
    let header = btable << aboves 
            [ besides [ td << "Problem", td << show p ]
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
    let belly = btable << aboves 
           [ besides [ td << "Matrikel", td << pre << show m ]
	   , besides [ td << "Key", td << pre << show k ]
	   , besides [ td << "Instanz", td << pre << show (toDoc i) ]
	   , besides [ td << "Description", td << pre << show desc ]
	   , besides [ td << "Initial", td << pre << show ( toDoc ini ) ]
	   ]


    let desc = pre << ( show $ describe p i )
    log <- editor "solution" ini env $ \ b -> do
	   let (res, com :: Doc) = export $ do
		    evaluate' p i b
           return $ pre << show com

    return $ header 
	   +++ h3 << "Diese Konfiguration erzeugt folgende Aufgabe:" +++ belly 
	   +++ h3 << "Für den Studenten sieht das dann so aus:" +++ desc +++ log

---------------------------------------------------------------------------------

page par contents = 
    let 
	heading = h2 << "Autotool-Super-Interface:"
		+++ primHtml "Aufgaben konfigurieren und testen"
	pref = preface par 
    in  header << thetitle << "Super.cgi"
            +++ body ( form ( foldr1 (+++) 
			      $ [ heading , hr , pref , hr ] ++ contents )
				   ! [ Text.Html.action "Super.cgi" , method "POST" ]
		 )
         
preface par = btable << aboves [ besides ( selector par 
  ++    [ td << submit click ( show Change ) ] ) ]

selector par =
    let opts = do
          let sel = lookup maker (env par)
	  Make doc _ _ <- makers par
          return $ Text.Html.option (primHtml doc) ! [ selected | Just doc == sel ]
    in  [ td << "choose Maker:" ,  td << select ! [ Text.Html.name maker ] << opts ]

btable x = table x ! [ border 1 ]