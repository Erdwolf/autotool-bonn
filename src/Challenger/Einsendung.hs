module Challenger.Einsendung where

--   $Id$

import Challenger.Ident

import Autolib.ToDoc

data Einsendung p i b
    = Aufgabe { problem::p, instanz::i, beweis::b}
    | Loesung { problem::p, ident :: Ident , beweis::b}
  deriving Read

    
instance (Show p, ToDoc i, ToDoc b) => ToDoc (Einsendung p i b) where 
  toDoc (e @ Aufgabe {}) = text "Aufgabe" <+> braces ( 
      fsep $ punctuate comma
      [ text "problem" <+> equals <+> text ( show (problem e))
      , text "instanz" <+> equals <+> toDoc (instanz e)
      , text "beweis" <+> equals <+> toDoc (beweis e)
      ])
  
  toDoc (e @ Loesung {}) = text "Loesung" <+> braces ( 
       fsep $ punctuate comma
       [ text "problem" <+> equals <+> text ( show (problem e))
       , text "ident" <+> equals <+> text ( show (ident e))
       , text "beweis" <+> equals <+> toDoc (beweis e)
       ]
      )
   

instance ToDoc (Einsendung p i b) => Show (Einsendung p i b) where 
  show = render . toDoc
