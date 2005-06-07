module Code.Check where

--  $Id$

import Code.Type
import Autolib.ToDoc
import Autolib.Set
import Data.FiniteMap
import Data.List
import Control.Monad
import qualified Autolib.Reporter.Checker as C
import qualified Autolib.Reporter.Set
import Autolib.Reporter

istotal :: ( ToDoc [b], ToDoc [a], ToDoc a, Ord a )
      => Set a
      -> Code a b
      -> Reporter ()
istotal xs code = do
    inform $ fsep 
	   [ text "ist", toDoc code, text "vollst�ndig"
	   , text "f�r", toDoc xs, text "?"
	   ]
    nested 4 $ Autolib.Reporter.Set.subeq
	     ( text "zu codierende Buchstaben" , xs )
	     ( text "tats�chlich codierte Buchstaben", mkSet $ keysFM code )

isprefix :: ( ToDoc [b], ToDoc [a], ToDoc a, Ord a, Eq b )
      => Code a b
      -> Reporter ()
isprefix code = do
    inform $ fsep [ text "ist", toDoc code, text "ein Pr�fix-Code?"]
    sequence_ $ do
        (x, cs) <- fmToList code
        (y, ds) <- fmToList code
        let msg (x, cs) = text "code" <+> parens ( toDoc x) 
                          <+> equals <+> toDoc cs
	guard $ x /= y
	return $ do
            when ( isPrefixOf cs ds ) $ reject $ fsep
	       [ text "Nein:"
               , msg (x, cs) , text "ist Pr�fix von", msg (y, ds)
               ]
    inform $ text "Ja."

