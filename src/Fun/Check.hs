{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Fun.Check where

--   $Id$

import Fun.Type
import qualified RAM.Builtin

import qualified Machine.Numerical.Config as C

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set

instance C.Check Property Fun where
    check ( Builtins allowed ) f = check_builtins ( mkSet allowed ) f

check_builtins :: Set Builtin -> Fun -> Reporter ()
check_builtins allowed f = do
    inform $ text "erlaubt sind diese Builtin-Funktionen:"
    inform $ nest 4 $ toDoc allowed
    let you = mkSet $ do
            Builtin i b <- subtrees f
            return b
    inform $ text "Sie benutzen:" <+> toDoc you
    let wrong = minusSet you allowed
    assert ( isEmptySet wrong ) $ text "sind alle zugelassen?"

subtrees :: Fun -> [ Fun ]
subtrees f = f : case f of
    Sub f gs -> concat $ map subtrees gs 
    PR  f gs -> concat $ map subtrees gs 
    Min f gs -> concat $ map subtrees gs 
    _        -> []


---------------------------------------------------------------------------

check_arity :: Int -> Fun -> Reporter () 
check_arity soll f = case f of

    -- Funktionen

    Zero ist -> do
	      diff f soll ist	      
    Succ ist -> do
	      when (1 /= ist) $ do
		   complain f
		   reject $ fsep [ text "Diese Funktion ist einstellig." ]
	      diff f soll ist
    Decr ist -> do
	      when (1 /= ist) $ do 
		   complain f 
		   reject $ fsep [ text "Diese Funktion ist einstellig." ]
	      diff f soll ist
    Proj ist unten -> do
              when ( ist < 0 ) $ do
	           complain f
		   reject $ fsep [ text "Es gibt keine", tupel ist ]
              when ( unten < 1 || unten > ist ) $ do
	           complain f
		   reject $ fsep [ text "Ein", tupel ist
				 , text "besitzt keine" , komponente unten
				 ]
	      diff f soll ist

    Builtin ist b -> do
        let ( arity, _ ) = RAM.Builtin.get b
	when ( arity /= ist ) $ do
	     complain f
	     reject $ fsep [ text "Diese Funktion ist"
			   , toDoc arity, text "-stellig." 
			   ]
	diff f soll ist

    -- Operatoren

    Sub ist [ ] -> do
        complain f
	reject $ fsep [ text "Der Sub-Operator benötigt wenigstens" 
		      , text "ein Argument." ]
    Sub ist (g : hs) -> do
        diff f soll ist
	let n = length hs
	check_arity n g
	mapM_ (check_arity soll) hs
        
    PR ist gh | 2 /= length gh -> do
        complain f
	reject $ fsep [ text "Der PR-Operator benötigt genau" 
		      , text "zwei Argumente." ]
    PR ist [ g, h ] -> do
        diff f soll ist
        check_arity (pred soll) g
	check_arity (succ soll) h
	
    Min ist gs | 1 /= length gs -> reject $ fsep 
	[ text "Der Min-Operator benötigt genau" , text "ein Argument." ]
    Min ist [ g ] -> do
        diff f soll ist
	check_arity (succ soll) g


complain :: Fun -> Reporter ()
complain f = do
    inform $ text "Fehler im Ausdruck:"
    inform $ nest 4 $ toDoc f

ste i = text "eine" <+> text ( show i ++ "-stellige" ) <+> text "Funktion"

diff f soll ist = when ( soll /= ist ) $ do
    complain f
    reject $ fsep
	      [ text "Dieser Ausdruck beschreibt", ste ist, text ","
	      , text "verlangt ist hier", ste soll, text "."
	      ]

tupel i = text $ show i ++ "-Tupel"
komponente i = text $ show i ++ "-te Komponente"
