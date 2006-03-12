--  $Id$

module Inter.Area where

import Gateway.Errmsg
import Inter.Env
import Inter.Click
import Autolib.Reader
import Autolib.ToDoc

import qualified Text.Html
import Text.Html ( pre, primHtml, (<<), (+++), (!)
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
       -> ( a -> IO Text.Html.Html )
       -> IO Text.Html.Html
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
	    
display :: Area -> Text.Html.Html
display a =  Text.Html.textarea ( Text.Html.primHtml $ contents a  ) 
              ! [ Text.Html.name $ (textarea_name a)
	        , Text.Html.rows $ show $ height a + 2
	        , Text.Html.cols $ show $ width  a
	        ]

-- | parse contents of area
-- * if success, then call continuation and display result
-- * if fail, display error message, don't call continuation
parser :: ( ToDoc a, Reader a )
       => Area 
       -> (a -> IO Text.Html.Html)
       -> IO Text.Html.Html
parser a continue = case parse (parse_complete reader) (name a) $ contents a of
	Left e -> do
	    return $ pre << show 
		   ( errmsg (width a) e 
	           $ contents a
		   )
        Right x -> do
            c <- continue x
 	    return $ primHtml "gelesen:" +++ pre << show (toDoc x) +++ c

buttons :: Area -> Text.Html.Html
buttons a = (<<) table $ aboves $ return $ Text.Html.besides $ do
               act <- [ Submit, Example ]
               return $ td <<  submit (button_name a) ( show act )