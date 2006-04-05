module Machine.Var where

--  $Id$

import Autolib.ToDoc
import Data.Typeable

data Var = Var Integer deriving ( Eq, Ord, Typeable )

instance ToDoc Var where
    toDoc (Var i) = text "x" <> toDoc i

