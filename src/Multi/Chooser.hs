module Multi.Chooser where

-- $Id$

import Multi.Config
import Multi.Selection

import CGI hiding ( span, map, div, head, name )
import qualified CGI

import Util.Datei
import Random

chooser :: Config -> IO ()
chooser conf = do
    its <- selection conf
    run [] $ choose_page conf its F0

choose_page :: Config -> [ Item ] -> F0 b -> CGI () 
choose_page conf its F0 = standardQuery "Choose" $ do
    radios <- items its
    submit (FL radios ) (answer_page its) empty

answer_page :: HasValue a => [Item] -> FL (a Int) VALID -> CGI ()
answer_page its (FL radios) = standardQuery "Answer" $ do
    sequence_ $ do
        ( it, radio ) <- zip its radios
	return $ tr $ do
	    let val = CGI.value radio
	    p $ text $ "you picked: "         ++ show val
	    p $ text $ "correct answer was: " ++ show ( solution it )

items :: [Item] -> WithHTML CGI [RadioGroup Int INVALID]
items its = table $ mapM item its

item :: Item -> WithHTML CGI (RadioGroup Int INVALID)
item it = tr $ do
    td $ img $ attr "src" ( picture it ) 
    td $ do
       radio <- radioGroup empty
       sequence_ $ do 
           (i, a) <- zip [0 .. ] $ alternatives it
	   return $ text a ## radioButton radio i empty
       return radio

    
    