module Util.Timeout (
    withTimeout
) where

import Config

import System.Timeout

withTimeout :: IO a -> IO a
withTimeout action = do
    result <- timeout timeLimit action
    case result of
        Just result -> return result
        Nothing     -> fail "time limit exceeded"
