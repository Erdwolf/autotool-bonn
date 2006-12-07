module Debug where

import System.IO
import System.Time
import qualified Local
import Control.Monad ( when )
import qualified System.Cmd

jetzt :: IO String
jetzt = do
    clock <- getClockTime
    cal <- toCalendarTime clock
    return $ calendarTimeToString cal

debug msg = when Local.debug $ do
    now <- jetzt
    appendFile "/tmp/tool.log" $ unlines [ now, msg ]
    -- hPutStrLn stderr cs
    -- hFlush stderr

system argv = do
    debug $ "start system: " ++ show argv
    res <- System.Cmd.system argv
    debug $ "  end system: " ++ show argv 
    debug $ "  with code: " ++ show res
    return res
