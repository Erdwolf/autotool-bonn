{-# OPTIONS -fglasgow-exts #-}

module RAM.Check where

--   $Id$

import RAM.Type
import RAM.Builtin
import RAM.Property

import qualified Machine.Numerical.Config as C

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set

instance C.Check Property Program where
    check No_While = no_while
    check No_Loop = no_loop
    check ( Builtins bs ) = builtins ( mkSet bs )

builtins :: Set Builtin -> Program -> Reporter ()
builtins allowed p = do
    inform $ text "erlaubt sind diese Builtin-Prozeduren:"
    inform $ nest 4 $ toDoc allowed
    let used = do
	    b @ ( Builtin {} ) <- flatten p
	    return b
    let you = mkSet $ do
	    b <- used
	    return $ name b
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"
    mapM_ check_arity used

check_arity :: Statement -> Reporter ()
check_arity b = do
    let ( ar, fun ) = get $ name b
    when ( length ( args b ) /= ar )
	 $ reject $ vcat [ text "Fehler in Anweisung" <+> toDoc b 
			 , fsep [ text "Das Builtin" , toDoc ( name b )
				, text "benötigt genau" ,toDoc ar
				, text "Argumente"
				]
			 , fsep [ text "aber Sie verwenden"
				, toDoc $ length $ args b
				]
			 ]


no_while :: Program -> Reporter ()
no_while p = do
    let whiles = do
	    w @ ( While {} ) <- flatten p
	    return w
    when ( not $ null whiles ) $ reject
	 $ text "Sie dürfen kein While benutzen."


no_loop :: Program -> Reporter ()
no_loop p = do
    let loops = do
	    w @ ( Loop {} ) <- flatten p
	    return w
    when ( not $ null loops ) $ reject
	 $ text "Sie dürfen kein Loop benutzen."

    