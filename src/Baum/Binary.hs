module Baum.Binary 

( make_quiz
)

where

--  $Id$

import Baum.Binary.Type
import Baum.Binary.Ops
import Baum.Binary.Show

import qualified Tree as T
import qualified Baum.Such.Central
import qualified Baum.Such.Class as C
import Inter.Types
import Autolib.ToDoc
import Data.Typeable

instance C.Such Baum where
    empty = Null
    isEmpty = isNull

    contains = contains
    insert = insert
    delete = delete

    equal = (==)
    contents = contents

instance T.ToTree Baum where
    toTree = toTree

data SuchbaumBinary = SuchbaumBinary 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag SuchbaumBinary Baum Int where
    tag = SuchbaumBinary

make_quiz :: Make
make_quiz = Baum.Such.Central.make_quiz SuchbaumBinary
