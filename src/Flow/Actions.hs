module Flow.Actions where

import Data.Set (Set)
import qualified Data.Set as S
import Autolib.TES.Identifier

class Actions a where actions :: a -> Set Identifier

instance Actions a => Actions [a] where
    actions = S.unions . map actions
