module NPDA.Sane where

--   $Id$

import NPDA.Type
import Autolib.Reporter
import qualified Autolib.Reporter.Checker as C
import Autolib.ToDoc

import Data.Maybe ( isJust )

sanity :: NPDAC x y z 
       => C.Type ( NPDA x y z )
sanity = C.Make 
       { C.nametag = ""
       , C.condition = text "Der Automat soll konsistent definiert sein."
       , C.investigate = sane
       }

sane :: NPDAC x y z
     => NPDA x y z 
     -> Reporter ()
sane a = do
    inform $ text "Sie haben eingesandt:"
    inform $ nest 4 $ toDoc a

    when ( isEmptySet $ eingabealphabet a ) 
	 $ reject $ text "das Eingabe-Alphabet ist leer"

    let s = startsymbol a
    when ( not $ s `elementOf` kelleralphabet a )
	 $ reject $ fsep
	 [ text "das Startsymbol",  toDoc s
	 , text "gehört nicht zum Kelleralphabet"
	 ]

    let z = startzustand a
    when ( not $ z `elementOf` zustandsmenge a )
	 $ reject $ fsep
	 [ text "der Startzustand"
	 , toDoc z
	 ,  text "gehört nicht zur Zustandsmenge"
	 ]

    case akzeptiert a of
        Zustand zs -> mapM_ ( \ z -> 
	     when ( not $ z `elementOf` zustandsmenge a)
		  $ reject $ fsep
		  [ text "der Endzustand", toDoc z
		  , text "gehört nicht zur Zustandsmenge"
		  ]
                            ) ( setToList zs )
        _ -> return ()

    let check_rule (l @ (mx, z, y), r @ (z',y')) = do
          let falsch msg = reject 
		 $ hsep [ text "Fehler in Regel", toDoc l, text "->", toDoc r ]
		 $$ nest 4 ( text msg )
          when ( isJust mx ) 
	       $ when ( not $ the mx `elementOf` eingabealphabet a )
		      $ reject
		      $ toDoc (the mx) <+> text " gehört nicht zum Eingabealphabet"
          mapM_ ( \ t -> when ( not $  t `elementOf` kelleralphabet a )
		      $ reject
		      $ toDoc t <+> text  " gehört nicht zum Kelleralphabet" )
	        ( y : y' )
          mapM_ ( \ t -> when ( not $ t `elementOf` zustandsmenge a )
		      $ reject
                      $ toDoc t <+> text " gehört nicht zur Zustandsmenge" )
	        [ z, z' ]

    mapM_ check_rule $ do
          (mxzy,zys) <- fmToList $ transitionen a
          (zy') <- setToList zys
	  return (mxzy,zy')

    inform $ text "Das ist wirklich ein nichtdeterministischer Kellerautomat."

