module JVM.Check where

-- $Id$

import JVM.Type
import JVM.Builtin

import Reporter
import ToDoc
import Sets


builtins :: Set Statement -> Program -> Reporter ()
builtins allowed p = do
    inform $ text "erlaubt sind diese Rechenbefehle:"
    inform $ nest 4 $ toDoc allowed
    let you = mkSet $ do
	    b <- flatten p
	    guard $ is_builtin b
	    return b
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"



    