module Boolean.Quiz where

--  $Id$

import qualified Boolean.Op as B
import TES.Type
import TES.Enum

conf :: Binu B.Op
conf = Binu
     { binary  = [ read "&&", read "||", read "<", read "==" ]
     , unary   = [ read "!" ]
     , nullary = [ read "true", read "false" ]
     }

roll :: Int -> IO ( Term () B.Op )
roll i = choose conf i


