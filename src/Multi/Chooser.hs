module Multi.Chooser where

-- -- $Id$

import Multi.Config
import Multi.Selection

import CGI hiding ( span, map, div, head, name )
import qualified CGI

import qualified Persistent2 as P

import Util.Datei
import Random
import IO

chooser :: Config -> IO ()
chooser conf = do
    run [] $ choose_page ( conf { tries_left = 0 } ) F0

persil = "Choose"

-- choose_page :: Config -> F0 b -> CGI () 
choose_page conf F0 = do
    Just hdl <- P.init persil []
    conf' <- if tries_left conf <= 0 
	then do its <- unsafe_io $ selection conf
	        P.set hdl its
		return $ conf { tries_left =        tries      conf }
	else do return $ conf { tries_left = pred $ tries_left conf }
    its <- P.get hdl
    standardQuery "Choose" $ do
	 p $ text $ unwords [ "for this instance, you have" 
	                    , show (tries_left conf'), "more tries."
			    ]
         radios <- items its
	 submit (FL radios ) (answer_page conf' its) empty

-- answer_page :: HasValue a => Config -> [Item] -> FL (a Int) VALID -> CGI ()
answer_page conf its (FL radios) = standardQuery "Answer" $ do
    sequence_ $ do
        ( it, radio ) <- zip its radios
	return $ tr $ do
	    let val = CGI.value radio
	    p $ text $ "you picked: "         ++ show val
	    p $ text $ "correct answer was: " ++ show ( solution it )
    submit F0 (choose_page conf) empty

items :: [Item] -> WithHTML CGI [RadioGroup Int INVALID]
items its = table $ do
    attr "frame" "border"
    attr "border" "2"
    mapM item its

item :: Item -> WithHTML CGI (RadioGroup Int INVALID)
item it = tr $ do
    td $ img $ attr "src" ( picture it ) 
    td $ table $ do
       attr "frame" "border"
       attr "border" "2"
       radio <- radioGroup empty
       sequence_ $ do 
           (i, a) <- zip [0 .. ] $ alternatives it
	   return $ tr $ td $ radioButton radio i empty ## text a
       return radio

    
    