module Debug where

import System.IO
import qualified Local
import Control.Monad ( when )

debug msg = when Local.debug $ do
    let cs = msg ++ "\n"
    appendFile "/tmp/tool.log" $ cs
    hPutStrLn stderr cs
    hFlush stderr
