module Baum.ZweiDrei 

( make_quiz
)

where

--  $Id$

import Baum.ZweiDrei.Type
import Baum.ZweiDrei.Ops
import Baum.ZweiDrei.Show

import qualified Baum.Such.Class as C
import qualified Baum.Such.Central

import Autolib.ToDoc
import Inter.Types
import Data.Typeable


instance C.Such Baum where
    empty = Null
    isEmpty = isNull

    contains = contains
    insert = insert
    delete = error "Zweidrei.delete nicht implementiert"

    equal = (==)
    contents = contents
    toTree = toTree

data SuchbaumZweiDrei = SuchbaumZweiDrei 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag SuchbaumZweiDrei Baum Int where
    tag = SuchbaumZweiDrei

make_quiz :: Make
make_quiz = Baum.Such.Central.make_quiz SuchbaumZweiDrei

