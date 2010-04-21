module Util.Parse (
    parseHelper
) where

import Util.Description

import Types.Description

import Autolib.Reader
import Autolib.Output
import Gateway.Errmsg

import Control.Monad.Error

parseHelper
    :: Reader b
    => SourceName -> [Char] -> ErrorT Description IO b
parseHelper what input = case parse (parse_complete reader) what input of
    Right res ->
        return res
    Left err -> do
        output <- liftIO $ fromOutput . Pre $ errmsg 80 err input
        throwError output
