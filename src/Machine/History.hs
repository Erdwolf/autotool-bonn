module Machine.History where

--   $Id$

import Autolib.ToDoc

detail :: Int
detail = 7

class History conf where
    -- | ergibt den Pfad im Berechnungsbaum, der zu der Konfiguration führt
    -- (in falscher reihenfolge, d. h. wurzel des baums kommt zuletzt)
    -- konfiguration selbst ist nicht in der liste
    history :: conf -> [ conf ]

present :: ( ToDoc conf, History conf ) 
	=> conf -> Doc
present conf = 
    let cs = conf : history conf
        ( rnew, mo  ) = splitAt detail $ cs
	( old, mid ) = splitAt detail $ reverse mo 
    in  if null mid
	then vcat [ text "Die Rechnung verläuft so:"
	          , nest 4 $ vcat $ map toDoc $ reverse cs
		  ]
	else vcat [ text "Die Rechnung beginnt so:"
		  , nest 4 $ vcat $ map toDoc $ old
		  , text "Die Rechnung endet so:"
		  , nest 4 $ vcat $ map toDoc $ reverse rnew
		  ]

