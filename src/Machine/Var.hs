module Machine.Var where

--  $Id$

import Autolib.ToDoc

data Var = Var Integer deriving ( Eq, Ord )

instance ToDoc Var where
    toDoc (Var i) = text "x" <> toDoc i

