-- $Id$

module JVM.Builtin 

( is_builtin 
)

where

import JVM.Type
import Sets

builtins :: Set Statement
builtins = mkSet [ Add, Sub, Mul ]

is_builtin :: Statement -> Bool
is_builtin s = s `elementOf` builtins
