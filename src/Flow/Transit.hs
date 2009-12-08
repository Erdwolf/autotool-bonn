{-# language TypeSynonymInstances, PatternSignatures #-}

module Flow.Transit where

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
