module Boolean.Equiv where

--  $Id$

import Boolean.Op
import Boolean.Eval

import Autolib.Reporter.Type
import Autolib.Set
import Autolib.ToDoc
import Control.Monad ( when )

-- | check equivalence
-- slightly asymmetric since
-- each variable of f has to occur in e 
-- (but not necessarily vice versa)
check :: Exp Bool -> Exp Bool -> Reporter ()
check e f = do
    inform $ vcat
	   [ text "Sind die Ausdrücke"
	   , nest 4 $ toDoc e
	   , text "und"
	   , nest 4 $ toDoc f
	   , text "äquivalent?"
	   ]
    mapM_ ( test e f ) $ belegungen $ vars e
    inform $ text "Ja, die Werte stimmen für alle Variablen-Belegungen überein."

test :: Exp Bool -> Exp Bool -> Belegung Identifier
     -> Reporter ()
test e f b = do
    x <- eval b e
    y <- eval b f
    when ( x /= y ) $ reject $ vcat
	 [ text "Nein. Bei Belegung"
	 , nest 4 $ toDoc b
	 , text "ist der Wert von"
	 , nest 8 $ toDoc e
	 , nest 4 $ text "gleich" <+> toDoc x
	 , text "aber der Wert von"
	 , nest 8 $ toDoc f
	 , nest 4 $ text "gleich" <+> toDoc y
	 ]




