module Algebraic.Class 

( Algebraic (..)
, module Expression.Op
, module Autolib.Reporter
, module Autolib.ToDoc
)

where

import Algebraic.Instance

import Condition
import Expression.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Choose
import Autolib.Size
import qualified Autolib.TES.Binu as B
import qualified Autolib.TES.Enum as E

import Data.Typeable

class (      Typeable tag
      , Show tag, Read tag
      , Ops a, Reader a  , ToDoc a  , Typeable a -- , Size a
      ) => Algebraic tag a | tag -> a where
    evaluate         :: tag -> Exp a -> Reporter a

    present	     :: tag -> a -> Reporter ()
    present tag it = return ()

    equivalent       :: tag -> a -> a -> Reporter Bool
    some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    default_operators :: tag -> B.Binu ( Op a )
    default_instance  :: tag -> Algebraic.Instance.Type a
