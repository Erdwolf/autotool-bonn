module Code.Check where

--  $Id$

import Code.Type
import Autolib.ToDoc
import Autolib.Set
import Data.FiniteMap
import Data.List
import Control.Monad
import qualified Autolib.Reporter.Checker as C
import qualified Autolib.Reporter.Subset
import Autolib.Reporter

istotal :: ( ToDoc [b], ToDoc [a], ToDoc a, Ord a )
      => Set a
      -> Code a b
      -> Reporter ()
istotal xs code = do
    inform $ fsep 
	   [ text "ist", toDoc code, text "vollständig"
	   , text "für", toDoc xs, text "?"
	   ]
    nested 4 $ Autolib.Reporter.Subset.check 
	     ( text "zu codierende Buchstaben" , xs )
	     ( text "tatsächlich codierte Buchstaben", mkSet $ keysFM code )

isprefix :: ( ToDoc [b], ToDoc [a], ToDoc a, Ord a, Eq b )
      => Code a b
      -> Reporter ()
isprefix code = do
    inform $ fsep [ text "ist", toDoc code, text "ein Präfix-Code?"]
    sequence_ $ do
        (x, cs) <- fmToList code
        (y, ds) <- fmToList code
	guard $ x /= y
	return $ do
            when ( isPrefixOf cs ds ) $ reject
	       $ text "Nein:" <+> toDoc (x, cs) <+> toDoc (y, ds)
    inform $ text "Ja."

