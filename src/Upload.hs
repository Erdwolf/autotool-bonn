-- | Eine Aufgabe, bei der Studenten L�sung im Klartext eintragen k�nnen.
-- Tutor soll dann "von Hand" die Oks eintragen

module Upload where

import Inter.Types
import Autolib.ToDoc
import Autolib.Xml
import Autolib.Reader
import Autolib.Reporter
import qualified Challenger as C
import Data.Typeable

data Upload = Upload deriving ( Eq, Ord, Show, Read, Typeable )

data Contents = Contents String deriving Typeable
instance Container Contents String where
    label _ = "Contents"
    pack (Contents cs) = cs
    unpack cs = Contents cs

instance ToDoc Contents where
    toDoc (Contents cs) = text cs
instance Reader Contents where
    reader = do cs <- getInput ; setInput [] ; return $ Contents cs

instance C.Partial Upload () Contents where
    report Upload () = inform $ vcat
	 [ text "Sie k�nnen hier Ihre L�sung als Text eingeben."
	 , text "Diese wird jetzt scheinbar abgeleht,"
	 , text "aber gespeichert und sp�ter (off-line) korrigiert."
	 , text "Die Punkte sehen Sie danach in der Datenbank."
	 ]
    initial Upload () = Contents "(Ihr Text hier.)"

    total_neu Upload () _ = do
        inform $ text "L�sung wird off-line bewertet."
        return $ C.Pending


instance C.Measure Upload () Contents where
    measure _ _ _ = 1

make :: Make
make = direct Upload ()
