module Inter.Error where

-- -- $Id$

import qualified Inter.Param as P
import qualified Inter.Types as T

import CGI
import ToDoc ( toDoc )

error_handler par cont = standardQuery "Unbekannte Aufgabe" $ do
    p $ do text $ "Die Aufgabe/Variante"
           pre  $ text $ P.problem par ++ "-" ++ P.variant par
           text $ "ist nicht bekannt oder nicht aktiviert."
    p $ do text $ "Folgende Aufgaben/Varianten sind bekannt:"
	   pre  $ text $ show $ toDoc $ do
	       T.Variant v <- P.variants par
	       return ( show $ T.problem v , T.variant v )
    cont
