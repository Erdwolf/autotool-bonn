-- | Datenstruktur für PCP-Aufgaben
--
-- autor Markus Kreuz
-- mai99byv@studserv.uni-leipzig.de

-- patches joe@informatik.uni-leipzig.de

module PCP.Type (
      module Iso
    , module Number
    , PCP (..)
    , module ToDoc
    ) where

import ToDoc
import Iso
import Number

data PCP = PCP [(String, String)] deriving (Show,Read)

-- ToDoc Instanz
instance ToDoc PCP where
	toDoc (PCP pcp) = text "PCP" <+> toDoc pcp

-- Isomorphie Instanz
-- !!!!!!!!!!!!Muss noch fertig gestellt werden
instance Iso (PCP) where
	iso pcp1 pcp2 = False


instance Number PCP PCP where
   number = id
