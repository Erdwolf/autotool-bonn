module Inter.Wrapper where

-- $Id$

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
    let i = T.gen_i v k

    Reporter.Result.wrapper $ do
        inform $ fsep
	    [ text "Ihre Einsendung zur Aufgabe"
	    , text $ T.variant v
	    , text "ist:"
	    ]
        inform $ nest 4 $ toDoc b

        C.partial p i b
        C.total   p i b
        return $ size b

