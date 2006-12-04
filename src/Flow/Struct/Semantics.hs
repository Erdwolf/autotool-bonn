module Flow.Struct.Semantics where

import Flow.Program
import Flow.Expression
import Flow.Struct.Data
import Flow.Action
import Flow.Auto

import Autolib.FiniteMap
import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.NFA as N
import qualified Autolib.Relation

import Autolib.ToDoc
import Autolib.Reporter

import Control.Monad.State

data ST = ST 
     { transitions :: [ ( Int, Maybe Action, Int ) ]
     , top :: Int
     }

next :: StateT ST Reporter Int
next = do
    st <- get
    let t = top st
    put $ st { top = succ t }
    return t

transit :: (Int, Maybe Action, Int ) -> StateT ST Reporter ()
transit this = do
    st <- get
    put $ st { transitions = this : transitions st }

semantics :: Program Statement -> Reporter ( N.NFA Action Int )
semantics p = 
    evalStateT ( program p ) $ ST { transitions = [], top = 0 }

program ( Program sts ) = do
    start <- next
    goal  <- next
    handle ( start, Block sts, goal )
    st <- get
    let enfa = eps_builder start goal $ transitions st
    return $ uneps enfa

handle ( start, stmt, goal ) = case stmt of
    Atomic this -> transit ( start, Just $ Execute this, goal )
    Block [] -> transit ( start, Nothing, goal )
    Block [ st ] -> handle ( start, st, goal )
    Block ( st : sts ) -> do
       mid <- next
       handle ( start, st, mid)
       handle ( mid,  Block sts , goal) 
    Branch c yes mno -> do
       p <- next
       transit ( start, condition c, p )
       handle ( p, yes, goal)
       case mno of
           Nothing -> return ()
	   Just no -> do
	       q <- next
	       transit ( start, reverse_condition c, q )
	       handle ( q, no, goal ) 
    While c st -> do
        p <- next
	transit ( start, reverse_condition c, goal )
	transit ( start, condition c,  p )
	handle ( p, st, start )

condition ( Expression flag this ) = 
    Just $ Condition flag this

reverse_condition ( Expression flag this ) = 
    Just $ Condition ( not flag ) this


