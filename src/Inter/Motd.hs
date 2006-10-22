module Inter.Motd where

--   $Id$

import Text.XHtml hiding ( text  )
import qualified Control.Exception

contents :: IO Html
contents = motd location


location :: FilePath
location = "/home/autotool/autotool/config/motd.html"

motd :: FilePath -> IO Html
motd file = do
    contents <- readFile file `Control.Exception.catch` \ err -> return ""
    return $ p << primHtml contents

