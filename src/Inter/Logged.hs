module Inter.Logged where


--  $Id$

import IO



logged :: String -> IO a -> IO a
logged msg act = do
    -- hPutStrLn stderr ( msg ++ " ... " ) ; hFlush stderr
    x <- act
    -- hPutStrLn stderr ( " ... " ++ msg ) ; hFlush stderr
    return x

