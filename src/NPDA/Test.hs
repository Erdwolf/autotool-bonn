module NPDA.Test

-- -- $Id$

( Config (..), Mod (..)
, test
)

where

import NPDA.Type
import NPDA.Det
import NPDA.Konfiguration

import NPDA.Vorganger
import NPDA.Akzeptieren
import NPDA.Vorrechnen

import Reporter
import Monad (guard)
import ToDoc
import Size

data Config = Config 
     { tiefe :: Int
     , det   :: Bool -- True: muß det. sein, False: egal	
     , modus :: Mod
     }
     deriving (Eq, Ord, Show)
     
data Mod = Egal | Keller | Zust 
     deriving (Eq, Ord, Show)

test
    :: (Ord x, Ord y, Ord z
       , Show x, Show y, Show z
       , ToDoc x, ToDoc y, ToDoc z
       , ToDoc [x], ToDoc [y], ToDoc [z]
       , ToDoc (Maybe x)
    ) 
    => Config
    -> [[x]] -> [[x]]
    -> NPDA x y z 
    -> Reporter Int

test conf pos neg a = do

     sane a

     when ( det conf ) $ ist_deterministisch a 

     when ( Keller == modus conf ) 
	  $ case akzeptiert a of
	        Leerer_Keller -> return ()
	        _ -> reject $ text 
		     $ "Der Automat soll aber durch leeren Keller akzeptieren."

     when ( Zust == modus conf ) 
	  $ case akzeptiert a of
	        Zustand zs -> return ()
	        _ -> reject $ text 
		     $ "Der Automat soll aber durch Endzustände akzeptieren."

     newline
     inform $ vcat $ map text
	[ "bei allen folgenden Rechnungen berücksichtige ich"
	, "nur die ersten " ++ show (tiefe conf) ++ " (durch Breitensuche)"
	, "erreichbaren Konfigurationen,"
	, "und auch davon nur die, bei denen das Kellerwort"
	, "nicht länger als 2 * (Länge der noch zu lesenden Eingabe) + 3 ist"
	]

     newline
     inform $ text "ich starte den Automaten auf einigen Wörtern aus L"
     let (offen, geheim) = splitAt 2 pos
     vorrechnens a $ offen
     newline
     inform $ text "weitere Eingaben:" <+> toDoc  geheim

     positiv_liste (tiefe conf) a pos

     newline
     inform $ text "ich starte den Automaten auf einigen Wörtern nicht in L"
     let (offen, geheim) = splitAt 2 neg
     vorrechnens a $ offen
     inform $ text "weitere Eingaben: " <+> toDoc geheim

     negativ_liste (tiefe conf) a neg

     return $ size a



