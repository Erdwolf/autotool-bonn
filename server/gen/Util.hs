module Util (
) where

import Data.String
import Text.PrettyPrint.HughesPJ

instance IsString Doc where
    fromString = text
