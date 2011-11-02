module Inter.Language where

import Autolib.Multilingual
import Gateway.CGI

choose :: Monad m => Form m Language
choose = do
    open table
    l <- click_choice_with_default 0 "choose language" $ do
        l <- [ minBound .. maxBound ]
        return ( show l, l )
    close
    return l

