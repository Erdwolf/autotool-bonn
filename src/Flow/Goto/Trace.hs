module Flow.Goto.Trace where

import Flow.Goto.Data
import Flow.Program
import Flow.Trace
import Flow.State
import Flow.Conditions
import Flow.Expression
import Flow.Action
import Autolib.TES.Identifier

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map ( Map)
import qualified Data.Map as M
import Autolib.Util.Size

import Data.List ( tails )
import Control.Monad ( guard )

jump_targets :: Program Statement 
             -> Map Identifier [ Statement ]
jump_targets ( Program ss ) = M.fromList $ do
    here @ ( Statement ( Just label ) _  : rest )
        <- tails ss
    return ( label, here )

traces :: Int -> Program Statement -> [ Trace ]
traces steps p @ ( Program ss ) = do
    let all = all_states $ conditions p 
    s <- all
    statements all ( jump_targets p ) ( start s ) steps ss 

statements all jumps t steps [] = do
    return $ t
statements all jumps t steps (Statement _ a :xs) = do
    guard $ complete t
    case a of
        Skip -> do
            statements all jumps t steps xs
        Action action -> t { complete = False } : do
            n <- all
            guard $ size t < steps
            let t' = extend n ( Execute action ) t
            statements all jumps t' steps xs
        Goto goal -> do
            let Just ss = M.lookup goal jumps
            statements all jumps t steps ss
        If_Goto test goal -> do
            let Just yeah = M.lookup goal jumps
            ( f, cont ) <- [ (False, xs),(True, yeah) ]
            guard $ f == evaluate ( current t ) test
            statements all jumps t steps cont


    

