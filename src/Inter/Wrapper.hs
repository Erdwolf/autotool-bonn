module Inter.Wrapper where

-- -- $Id$

import qualified Inter.Types as T
import qualified Challenger  as C
import qualified Reporter.Result

import ToDoc
import Size
import Reporter

wrapper :: T.V p i b
       => T.Var p i b 
       -> String -- matrikel
       -> b  -- input
       -> IO ( Maybe Int )
wrapper v mat b = do
    let p = T.problem v
    k <- T.key  v mat
    generator <- T.gen v k

    Reporter.Result.wrapper $ do
        inform $ fsep
	       [ text "Sie bearbeiten die Aufgabe" , text $ T.aufgabe v
	       , parens ( text "Version:" <+> text ( T.version v ) )
	       ]
        i <- generator

	inform $ text "Ihre Einsendung ist:"
        inform $ nest 4 $ toDoc b

        C.partial p i b
        C.total   p i b
        return $ size b

