module Finite.Type where

import ToDoc
import Set

data Program q a 
    = Program { statements :: [ Statement q a ]
	      -- schreibe nachricht, wenn einer dieser Werte geschrieben 
	      , watch :: Set String
	      -- schreiben nachricht, wenn eines dieser labels erreicht
	      , trace :: Set String
	      }

instance ToDoc ( Statement q a ) => ToDoc ( Program q a ) where
    toDoc p = text "Program" <+> ( fsep $ punctuate comma
	  [ text "statements" <+> equals <+> toDoc (statements p)
	  , text "watch" <+> equals <+> toDoc (watch p)
	  , text "trace" <+> equals <+> toDoc (trace p)
	  ] )

data Statement q a 
    = Label String
    | Assign String ( Value q )
    | If ( Value q, Value q ) String
    | Goto String
    | Accept
    | Reject
    | Do a
  deriving ( Eq, Ord, Show, Read )

data Value q 
    = Const String
    | Ref String
    | Ask q
  deriving ( Eq, Ord, Show, Read )

instance ( Show q, Show a ) => ToDoc ( Statement q a )
    where toDoc = text . show
instance Show q => ToDoc ( Value q )
    where toDoc = text . show


