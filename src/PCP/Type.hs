-- | Datenstruktur für PCP-Aufgaben
--
-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

module PCP.Type (
	  module Iso
    , PCP (..)
    , module ToDoc
	)where

import ToDoc
import Iso

data PCP a = PCP [(a,(String, String))] deriving (Show,Read)

-- ToDoc Instanz
instance (Show a) => ToDoc (PCP a) where
	toDoc (PCP pcp) = text "PCP" <+> text (show pcp)

-- Isomorphie Instanz
-- !!!!!!!!!!!!Muss noch fertig gestellt werden
instance (Eq a, Ord a) => Iso (PCP a) where
	iso pcp1 pcp2 = False
