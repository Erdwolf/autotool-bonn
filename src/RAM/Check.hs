module RAM.Check where

-- $Id$

import RAM.Type
import RAM.Builtin

import Reporter
import ToDoc
import Sets

builtins :: Set Builtin -> Program -> Reporter ()
builtins allowed p = do
    inform $ text "erlaubt sind diese Builtin-Prozeduren:"
	   <+> toDoc allowed
    let you = mkSet $ do
	    b @ ( Builtin {} ) <- flatten p
	    return $ name b
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"

    -- todo: teste arity der builtins

loopy :: Program -> Reporter ()
loopy p = do
    let whiles = do
	    w @ ( While {} ) <- flatten p
	    return w
    when ( not $ null whiles ) $ reject
	 $ text "Sie dürfen kein While benutzen."

    