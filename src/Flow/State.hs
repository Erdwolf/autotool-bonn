module Flow.State 

( State 
, lookup
, all_states
)

where

import Prelude hiding ( lookup )

import Autolib.TES.Identifier
import Autolib.Hash

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Util.Size

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad ( forM )

import GHC.Int

-- | possible status of tests (predicates)
data State = State
    { hashcode :: Int32
    , contents :: Map Identifier Bool
    }
    deriving ( Eq, Ord )

instance Hash State where hash = hashcode

instance Size State -- who needs this?

instance ToDoc State where
    toDoc s = text "state" 
            <+> text ( show $ M.toList $ contents s )

instance Reader State where
    reader = do
        my_reserved "state"
        m <- reader
        return $ state m

state :: [ (Identifier, Bool) ] -> State
state m = State 
        { contents = M.fromList m
        , hashcode = hash m
        }

lookup :: Identifier -> State -> Maybe Bool
lookup i s = M.lookup i $ contents s

all_states :: Set Identifier -> Set State 
all_states s = S.fromList $ map state $
    forM ( S.toList s ) $ \ v -> [ (v,False), (v,True) ]
