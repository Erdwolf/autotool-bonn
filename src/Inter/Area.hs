--  $Id$

module Inter.Area where

import Inter.Errmsg
import Autolib.Reader
import Autolib.ToDoc

import qualified Text.Html
import Text.Html ( pre, primHtml, (<<), (+++), (!))

data Area =
     Area { name :: String
          , fallback :: String
	  , contents :: String
	  , height :: Int
	  , width :: Int
	  }

make :: String -> String -> Area
make nm cs = 
    Area { name = nm
         , fallback = cs
	 , contents = cs
	 , height = length ( lines cs )
	 , width = 60
	 }

display :: Area -> Text.Html.Html
display a =  Text.Html.textarea ( Text.Html.primHtml $ contents a  ) 
              ! [ Text.Html.name $ name a
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
	    return $ Text.Html.primHtml 
		   $ show 
		   $ errmsg (width a) e 
	           $ contents a
        Right x -> do
            c <- continue x
 	    return $ primHtml "gelesen:" +++ pre << show (toDoc x) +++ c
