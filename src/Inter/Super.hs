-- | to edit the problem configuration
-- and to check a sample solution

--  $Id$

module Main where

import Inter.Make
import Inter.Env
import Challenger.Partial
import Inter.Types
import Inter.Click
import Inter.Area
import Inter.Errmsg

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

-- for testing:
import qualified HTWK.SS04.Informatik.Boolean as B

data Par =
     Par { env :: Env
	 , makers :: [ Make ]
	 , maker :: Make
	 , config :: Area
	 , solution :: Area
	 }

main :: IO ()
main = wrapper $ \ e ->  
         iface ( Par { makers = B.makers 
		     , maker = error "Super.maker"
		     , config = error "Super.config"
		     , solution = error "Super.solution"
		     , env = e
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
		  c <- core $ par { maker = mk 
				  , config = Inter.Area.make "input" inp
				  } 
		  return $  h3 << ( "Chosing Maker: " ++ show mcs ) +++ c 
          _    -> return $ h3 << ( "More than one Maker with name: " ++ show mcs )

    return $ page par [ presenter, center ]

core :: Par -> IO Html 
core par = case maker par of 
    Make doc fun ex -> do    
        let ty = primHtml "has type"
	        +++ pre << show ( toDoc $ typeOf fun )

        let scratch = Inter.Area.display ( config par )

	let buttons name = (<<) table $ aboves $ return $ besides $ do
               act <- [ Submit, Previous, Example, Reset ]
               return $ td <<  submit name ( show act )

        va <- Inter.Area.parser (config par) $ \ conf -> do
                    h <- handler par ( fun conf )
		    return h

        return $ foldr1 (+++) 
	       $ intersperse br
               $ [ ty, scratch, buttons "Click", va ]

handler :: ( Reader b, Partial p i b )
	=> Par
	-> Var p i b 
	-> IO Html
handler par ( var :: Var p i b ) = do
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
    
    let sol = fromMaybe (show $ toDoc ini) $ lookup "solution" $ env par
	asol = Inter.Area.make "solution" sol
    log <- Inter.Area.parser asol $ \ ( x :: b ) -> do
              return $ primHtml "hier Inter.Evaluate aufrufen"

    return $ header +++ belly +++ log

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

