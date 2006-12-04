module Flow.Goto.Semantics where

import Flow.Program
import Flow.Expression
import Flow.Goto.Data
import Flow.Action
import Flow.Auto

import Autolib.FiniteMap
import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.NFA as N


import Autolib.ToDoc
import Autolib.Reporter

semantics :: Program Statement -> Reporter ( N.NFA Action Int )
semantics p @ ( Program stmts ) = do
    let nstmts = zip [ 0 .. ] stmts
    table <- collect_table nstmts
    transitions <- fmap concat
        $ mapM ( collect_transitions table ) nstmts
    let enfa = eps_builder 0 ( length stmts ) transitions
    return $ uneps enfa

collect_transitions table ns @ ( n, Statement _ body ) = case body of
    Action act -> do
        return [ ( n, Just $ Execute act, n+1 ) ]
    Goto target -> case lookupFM table target of
	Nothing -> reject $ text "undefined label in" <+> toDoc ns
	Just line -> return [ ( n, Nothing , line ) ]
    If_Goto (Expression noflip c) target -> case lookupFM table target of
	Nothing -> reject $ text "undefined label in" <+> toDoc ns
	Just line -> return 
	    [ ( n, Just $ Condition noflip  c , line )
	    , ( n, Just $ Condition (not noflip) c , n+1  ) 
	    ]

collect_table nstmts = foldM 
    ( \ fm ( n, st @ ( Statement ml body ) ) -> do
        case ml of
	    Nothing -> return fm
	    Just l  -> case lookupFM fm l of
	        Nothing -> return $ addToFM fm l n
		Just m  -> reject $ vcat
		    [ text "label" <+> toDoc l
		    , text "in statement" <+> toDoc st
		    , text "at position" <+> toDoc n
		    , text "already defined at position" <+> toDoc m
		    ]
    ) emptyFM nstmts
    


