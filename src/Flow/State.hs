module Flow.State where

import Autolib.TES.Identifier

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad ( forM )

-- | possible status of tests (predicates)
type State = Map Identifier Bool

all_states :: Set Identifier -> [ State ]
all_states s = fmap M.fromList $
    forM ( S.toList s ) $ \ v -> [ (v,False), (v,True) ]
