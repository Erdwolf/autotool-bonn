module Flow.Goto.Semantics where

import Flow.Program
import Flow.Expression
import Flow.Conditions
import Flow.State
import Flow.Goto.Data
import Flow.Action
import Flow.Auto

import Flow.Transit

import Autolib.FiniteMap
import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.NFA as N
import Autolib.NFA.Shortest

import Autolib.ToDoc
import Autolib.Reporter

import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad.State hiding ( State )
import Data.List ( partition )

semantics :: Set Flow.State.State
          -> Program Statement 
          -> Reporter ( N.NFA Label Vertex )
semantics all p @ ( Program stmts ) = 
    evalStateT ( program (S.toList all) p ) 
        $ ST { transitions = [], top = length stmts }


program all p @ ( Program sts ) = do
    let start = 0
        nsts = zip [ start .. ] sts
    labels <- lift $ collect_labels nsts
    final <- next -- risky coding: top has been
             -- initialized to (length stmts) above,
             -- then the last of the (n+1) targets below
             -- leads to this final destination

    let with_label lab f = case M.lookup lab labels of
            Nothing -> lift $ reject $ hsep 
                 [ text "label", toDoc lab, text "nicht gefunden" ]
            Just n -> f n

    forM nsts $ \ ( n, Statement _ a ) -> case a of
        Skip -> forM_ all $ \ s -> 
            transit ((n, s), Nothing, (n+1, s))
        Action a -> forM_ all $ \ s -> forM all $ \ s'->
            transit ((n,s), Just(s, Execute a),(n+1,s'))
        Goto lab -> forM_ all $ \ s -> 
            with_label lab $ \ n' -> 
            transit ((n,s),Nothing,(n',s))
        If_Goto test lab -> with_label lab $ \ n' -> do
            let (yeah,noh) = 
                    partition 
                        ( \ st -> evaluate st test ) 
                        all
            forM_ yeah $ \ s -> 
                transit ((n,s),Nothing,(n',s))
            forM_ noh $ \ s -> 
                transit ((n,s),Nothing,(n+1,s))
    halt <- next
    forM all $ \ s -> 
        transit ((final,s),Just(s,Halt),(halt,s))

    let times :: [a] -> [b] -> [(a,b)]
        times xs ys = xs >>= \ x -> ys >>= \ y -> [(x,y)]
    st <- get
    let enfa = eps_builder_all_final
                   (times [start] all)
                   ( transitions st )
    return $ uneps enfa


------------------------------------------------

collect_labels nsts = foldM
    ( \ fm ( n, st @ (Statement mlab _ )) -> case mlab of
	    Nothing -> return fm
	    Just l  -> case lookupFM fm l of
	        Nothing -> return $ addToFM fm l n
		Just m  -> reject $ vcat
		    [ text "Label" <+> toDoc l
		    , text "in Anweisung" <+> toDoc st
		    , text "an Position" <+> toDoc n
		    , text "bereits definiert an Position" <+> toDoc m
		    ]
    ) emptyFM nsts


    


