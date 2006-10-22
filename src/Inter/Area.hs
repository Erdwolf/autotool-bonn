--  $Id$

module Inter.Area where

import Gateway.Errmsg
import Inter.Env
import Inter.Click
import Autolib.Reader
import Autolib.ToDoc

import qualified Text.XHtml
import Text.XHtml ( pre, primHtml, (<<), (+++), (!)
		 , table, aboves, besides, td, submit, br, h3
		 )

import Data.Maybe
import Control.Monad

data Area =
     Area { name :: String
	  , contents :: String
	  , height :: Int
	  , width :: Int
	  }

button_ :: String -> String
button_ cs = "B" ++ cs

button_name = button_ . name

textarea_ :: String -> String
textarea_ cs = "T" ++ cs

textarea_name = textarea_ . name

make :: String -> String -> Area
make nm cs = 
    Area { name = nm
	 , contents = cs
	 , height = length ( lines cs )
	 , width = 80
	 }

-- | input area, submit button, log window
editor :: ( ToDoc a, Reader a )
       => String
       -> a -- ^ default value
       -> Env 
       -> ( a -> IO Text.XHtml.Html )
       -> IO Text.XHtml.Html
editor nm def env computer = do
    -- get contents of textarea if it is there and user has *not* clicked Example
    let dropin = show $ toDoc def
    let cs = if lookup (button_ nm) env == Just (show Example)
	     then dropin
	     else fromMaybe dropin $ lookup (textarea_ nm) env
    let a = make nm cs

    log <- parser a computer
    return $ foldr1 (+++)
	   [ h3 << ( "Editor/Logger for " ++ name a ) 
	   , br , display a , br , buttons a , br , log
	   ]
	    
display :: Area -> Text.XHtml.Html
display a =  Text.XHtml.textarea ( Text.XHtml.primHtml $ contents a  ) 
              ! [ Text.XHtml.name $ (textarea_name a)
	        , Text.XHtml.rows $ show $ height a + 2
	        , Text.XHtml.cols $ show $ width  a
	        ]

-- | parse contents of area
-- * if success, then call continuation and display result
-- * if fail, display error message, don't call continuation
parser :: ( ToDoc a, Reader a )
       => Area 
       -> (a -> IO Text.XHtml.Html)
       -> IO Text.XHtml.Html
parser a continue = case parse (parse_complete reader) (name a) $ contents a of
	Left e -> do
	    return $ pre << show 
		   ( errmsg (width a) e 
	           $ contents a
		   )
        Right x -> do
            c <- continue x
 	    return $ primHtml "gelesen:" +++ pre << show (toDoc x) +++ c

buttons :: Area -> Text.XHtml.Html
buttons a = (<<) table $ aboves $ return $ Text.XHtml.besides $ do
               act <- [ Submit, Example ]
               return $ td <<  submit (button_name a) ( show act )