obsolete module Robots.Type 

( module Robots.Data
, module Robots.Config 
, module Robots.Move
)

where

--  $Id$

import Robots.Data
import Robots.Config
import Robots.Move
import Robots.Nice

import Challenger.Partial
import Inter.Types
import Autolib.ToDoc

instance Partial Robots Config [ Zug ] where

    initial Robots k = [ ("A", N), ("B", O) ]
    -- partial Robots k zs = valid k
    total   Robots k zs = executes k zs







