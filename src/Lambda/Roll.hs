module Lambda.Roll where

import Lambda.Type
import Autolib.TES.Identifier
import Autolib.Set
import Autolib.Util.Zufall
import System.Random

term :: Int -> Set Identifier -> IO Lambda
term s vars = 
    if s <= 1 then variable s vars
    else composition s vars

variable s vars = do
    v <- eins $ setToList vars
    return $ Variable v

composition s vars = do
    action <- eins [ abstraction , application , application ]
    action s vars

abstraction s vars = do
    let v = next_free vars
    b <- term (s - 1) ( mkSet [v] `union` vars )
    return $ Abstract v b

application s vars = do
    sl <- randomRIO ( 1, s-1 )
    let sr = s - sl
    l <- term sl vars
    r <- term sr vars
    return $ Apply l r


    
