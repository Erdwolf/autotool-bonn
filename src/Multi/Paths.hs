module Multi.Paths where

-- -- $Id$

import Dot.Dot
import Util.Datei


basename :: FilePath -> FilePath
basename = reverse . takeWhile (/= '/') . reverse 		

public_html :: Datei -> Datei
public_html d = d { pfad = "public_html" : pfad d }

data_index :: Datei -> Datei
data_index d = d { pfad = "data" : pfad d, name = "index.text" }

deng :: ( ToDot a )
     => Datei -- ohne extension
     -> a 
     -> IO Datei -- mit extension
deng d a = do
    erzeugeVerzeichnisse d
    h <- home d
    f <- meng h a
    return $ d { name = basename f }

