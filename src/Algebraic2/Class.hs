module Algebraic2.Class 

( Algebraic (..)
, module Expression.Op
, module Autolib.Reporter
, module Autolib.ToDoc
)

where

import Algebraic2.Instance as AI

import Condition
import Expression.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Choose
import Autolib.Size
import Autolib.FiniteMap
import qualified Autolib.TES.Binu as B
import qualified Autolib.TES.Enum as E

import Data.Typeable

type Belegung a = FiniteMap Identifier a

class (      Typeable tag
      , Show tag, Read tag
      , Ops a, Reader a  , ToDoc a  , Typeable a -- , Size a
      , Reader context, ToDoc context, Typeable context
      ) => Algebraic tag context a | tag -> a, tag -> context where

    introduce :: tag -> context -> Reporter ()
    introduce tag con = return ()

    evaluate         :: tag -> Belegung a -> Exp a -> Reporter a

    evaluateC :: tag -> context -> Belegung a -> Exp a -> Reporter a
    evaluateC tag context = evaluate tag -- ignore context, for transition

    present	     :: tag -> a -> Reporter ()
    present tag it = return ()

    equivalent       :: tag -> a -> a -> Reporter Bool
    some_formula     :: tag -> AI.Type context a -> Exp a
    default_context :: tag -> context
    default_operators :: tag -> B.Binu ( Op a )
    default_predefined :: tag -> Belegung a
    default_instance  :: tag -> AI.Type context a
