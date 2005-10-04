module Debug where

import System.IO
import qualified Local
import Control.Monad ( when )
import qualified System.Cmd

debug msg = when Local.debug $ do
    let cs = msg ++ "\n"
    when ( 0 /= length cs ) $ appendFile "/tmp/tool.log" $ cs
    -- hPutStrLn stderr cs
    -- hFlush stderr

system argv = do
    debug $ "start system: " ++ show argv
    res <- System.Cmd.system argv
    debug $ "  end system: " ++ show argv 
    debug $ "  with code: " ++ show res
    return res
