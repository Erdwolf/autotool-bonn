{-# language TypeSynonymInstances, PatternSignatures #-}

module Flow.Struct.Semantics where

import Flow.Program
import Flow.Expression
import Flow.Conditions
import Flow.Struct.Data hiding ( branch )
import Flow.Action
import Flow.Auto
import Flow.State 

import Autolib.FiniteMap
import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.NFA as N
import qualified Autolib.Relation

import Autolib.NFA.Shortest ( accepted )

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Symbol

import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad.State hiding ( State )
import Data.List ( partition )


type Vertex = ( Int, State )
type Label = ( State, Action )
type Transition = ( Vertex, Maybe Label, Vertex )

instance Symbol Label -- 

data ST = ST 
     { transitions :: [ Transition ]
     , top :: Int
     }

next :: StateT ST Reporter Int
next = do
    st <- get
    let t = top st
    put $ st { top = succ t }
    return t

transit :: Transition -> StateT ST Reporter ()
transit this = do
    st <- get
    put $ st { transitions = this : transitions st }

semantics :: Program Statement 
          -> Reporter ( N.NFA Label Vertex )
semantics p = 
    evalStateT ( program p ) 
        $ ST { transitions = [], top = 0 }

program p @ ( Program sts ) = do
    start :: Int <- next
    goal  :: Int <- next
    let all :: [ State ] = all_states $ conditions p
    handles all ( start, sts, goal )
    final <- next;
    forM all $ \ st -> 
        transit ((goal,st),Just (st,Halt),(final,st))
    st <- get
    let times :: [a] -> [b] -> [(a,b)]
        times xs ys = xs >>= \ x -> ys >>= \ y -> [(x,y)]
    let enfa = eps_builder 
                   (times [start] all :: [Vertex])
                   (times [final] all :: [Vertex])
             $ transitions st
    return $ uneps enfa

---------------------------------------------------

-- | all possible paths from start to goal
handles all  ( start, [], goal ) = do
    forM_ all $ \ st ->
        transit ( (start,st), Nothing, ( goal, st ) )
handles all  ( start, [x], goal ) = do
    handle all  ( start, x, goal )
handles all  ( start, x:xs, goal ) = do
    mid <- next
    handle all  ( start, x, mid )
    handles all ( mid, xs, goal )

handle all  (start, stmt, goal) = case stmt of
    Atomic this -> 
        forM_ all $ \ st -> forM_ all $ \ st' ->
          transit 
            ((start,st)
            ,Just(st,Execute this) -- a real transition
            ,(goal,st'))
    Block xs -> 
        handles all  ( start,xs, goal)

    Branch {} -> branch all (start,stmt,goal,goal)

    While test x -> do
        -- need to introduce eps transition
        -- to make the transformation safe
        loop <- next
        forM_ all $ \ st -> 
            transit ((start, st),Nothing,(loop,st))

        branch all 
             (loop, Branch test x Nothing, loop, goal) 

branch all 
    (start,Branch test yes mno ,goal1, goal2) = do
        let (yeah,noh) = 
                partition ( \ st -> evaluate st test ) 
                          all
        yy <- next
        forM yeah $ \ st -> 
            transit ((start,st),Nothing,(yy,st))
        nn <- next
        forM noh  $ \ st -> 
            transit ((start,st),Nothing,(nn,st))

        handle all (yy,yes,goal1)
        let no = case mno of
                    Just no -> no ; Nothing -> Block []
        handle all (nn,no,goal2)
