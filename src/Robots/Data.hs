module Robots.Data where

-- $Id$

import ToDoc
import Number
import Iso

data Robots 
   = Robots deriving ( Show, Read )

type Position = ( Integer, Integer )


data Robot = Robot { name :: String
		   , position :: Position
		   , ziel :: Maybe Position
		   }
     deriving ( Read, Eq, Ord )

instance ToDoc Robot where
    toDoc sh = text "Robot" <+> braces ( fsep $ punctuate comma
	  [ text "name" <+> equals <+> toDoc ( name sh )
	  , text "position" <+> equals <+> toDoc ( position sh )
	  , text "ziel" <+> equals <+> toDoc ( ziel sh )
	  ] )

instance Show Robot where show = render . toDoc

instance Number Robot Robot where number = id

instance Iso Robot where iso = (==)


data Richtung = N | O | S | W 
     deriving ( Eq, Ord, Show, Read )

instance ToDoc Richtung where toDoc = text . show

type Zug = ( String, Richtung )

