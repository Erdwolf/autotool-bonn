module Flow.Conditions where


import Data.Set ( Set )
import qualified Data.Set as S

import Autolib.TES.Identifier

class Conditions p where 
    conditions :: p -> Set Identifier

instance Conditions s => Conditions [s] where
    conditions ss =  S.unions $ map conditions ss
