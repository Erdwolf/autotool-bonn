module Baum.Binary 

( module Baum.Binary.Type
, module Baum.Binary.Ops
)

where

--  $Id$

import Baum.Binary.Type
import Baum.Binary.Ops
import Baum.Binary.Show

import qualified Baum.Such.Class as C
import Autolib.ToDoc

instance C.Such Baum where
    empty = Null
    isEmpty = isNull

    contains = contains
    insert = insert
    delete = delete

    equal = (==)
    contents = contents
    form = toDoc . toTree

