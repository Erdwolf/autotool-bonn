module Util (
) where

import Data.String
import Text.PrettyPrint.HughesPJ

-- Provide IsString instance for Doc so we can write  "abc" <+> "def"
-- instead of  text "abc" <+> text "def"  with the OverloadedStrings
-- language extension.
instance IsString Doc where
    fromString = text
