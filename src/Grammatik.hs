module Grammatik 

( module Grammatik.Type
, GT
)

where

-- $Id$

import Grammatik.Type
import Grammatik.Trace

type GT = ( Grammatik, Tracks )
