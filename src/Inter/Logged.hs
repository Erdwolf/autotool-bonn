module Inter.Logged where


--  $Id$

import qualified Control.SQL 
import IO

logged :: String -> IO a -> IO a
logged msg act = do
    Control.SQL.logged ( msg ++ " ..." )
    x <- act
    Control.SQL.logged ( "... " ++ msg )
    return x

