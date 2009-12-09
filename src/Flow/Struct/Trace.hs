module Flow.Struct.Trace where

import Flow.Struct.Data
import Flow.Trace
import Flow.Action
import Flow.State
import Flow.Expression
import Flow.Program
import Flow.Conditions
import Flow.Conditions

import Autolib.Util.Size
import Control.Monad ( guard )

traces :: Int -> Program Statement -> [ Trace ]
traces steps p @ ( Program ss ) = do
    let all = all_states $ conditions p 
    s <- all
    statements all ( start s ) steps ss 

statements all t steps [] = do
    return $ t
statements all t steps (x:xs) = do
    guard $ complete t
    t <- statement all t steps x
    t { complete = False } : do
        guard $ complete t
        statements all t steps xs
    
statement all t steps s = case s of
    Atomic action -> do
        guard $ size t < steps
        n <- all
        return $ extend n ( Execute action ) t
    Block ss -> statements all t steps ss
    Branch test yeah mnoh -> do
        let noh = case mnoh of
                 Just noh -> noh
                 Nothing -> Block []
        ( f, cont ) <- [ (False, noh),(True, yeah) ]
        guard $ f == evaluate ( current t ) test
        statement all t steps cont
    While test body -> statement all t steps 
        $ Branch test ( Block [ body, s ] ) Nothing

