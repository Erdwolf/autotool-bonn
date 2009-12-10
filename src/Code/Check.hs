module Code.Check where

--  $Id$

import Code.Type

import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import qualified Autolib.Reporter.Checker as C
import qualified Autolib.Reporter.Set
import Autolib.Reporter

import Data.List
import Control.Monad

istotal :: ( ToDoc b,  ToDoc a, Ord a )
      => Set a
      -> Code a b
      -> Reporter ()
istotal xs c @ ( Code code ) = do
    inform $ fsep 
	   [ text "ist", toDoc c, text "vollständig"
	   , text "für", toDoc xs, text "?"
	   ]
    nested 4 $ Autolib.Reporter.Set.subeq
	     ( text "zu codierende Buchstaben" , xs )
	     ( text "tatsächlich codierte Buchstaben", mkSet $ keysFM code )

isprefix :: ( ToDoc a, ToDoc b, Ord a, Eq b )
      => Code a b
      -> Reporter ()
isprefix c @ ( Code code ) = do
    inform $ fsep [ text "ist", toDoc c, text "ein Präfix-Code?"]
    sequence_ $ do
        (x, cs) <- fmToList code
        (y, ds) <- fmToList code
        let msg (x, cs) = text "code" <+> parens ( toDoc x) 
                          <+> equals <+> toDoc cs
	guard $ x /= y
	return $ do
            when ( isPrefixOf cs ds ) $ reject $ fsep
	       [ text "Nein:"
               , msg (x, cs) , text "ist Präfix von", msg (y, ds)
               ]
    inform $ text "Ja."

