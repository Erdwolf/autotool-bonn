-- | Eine Aufgabe, bei der Studenten Lösung im Klartext eintragen können.
-- Tutor soll dann "von Hand" die Oks eintragen

module Upload where

import Upload.Config

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

instance C.Partial Upload Config Contents where
    report Upload conf = inform $ vcat
	 [ text $ aufgabe conf
	 , text ""
	 , text "erreichbare Punktzahl:" <+> toDoc ( punkte conf )
	 , text ""
	 , text "Sie können hier Ihre Lösung als Text eingeben."
	 , text "Diese wird jetzt scheinbar abgeleht,"
	 , text "aber gespeichert und später (off-line) korrigiert."
	 , text "Die Punkte sehen Sie danach in der Datenbank."
	 ]
    initial Upload conf = Contents "(Ihr Text hier.)"

    total_neu Upload conf _ = do
        inform $ text "Lösung wird off-line bewertet."
        return $ C.Pending


instance C.Measure Upload Config Contents where
    measure _ _ _ = 1

make :: Make
make = direct Upload Upload.Config.example

