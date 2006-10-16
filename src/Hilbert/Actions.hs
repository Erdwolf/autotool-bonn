module Actions

( Env
, look

, acts
)

where

import Ids
import Syntax

import Axioms
import Read
import Ops
import FiniteMap

import Env

type Value = Env -> (Exp -> Exp)
type Action = Env -> Env

----------------------------------------------------------------

value env (App fun [ orig, sub ] ) | fun == einsetz =
   apply (mkfm sub) (look env orig) 

value env x @ (App fun [ left, right ] ) | fun == mopo =
   let l = look env left
       r = look env right
   in  case r of
	    App fun [ pre, post ] | fun == implies ->
		if l == pre 
		then post
		else error $ "modus ponens: passt nicht: " ++ show x
	    _ -> error $ "modus ponens: keine Implikation: " ++ show x

----------------------------------------------------------------

update :: Env -> Id -> Exp -> Env
update env id exp =
    case lookupFM env id of
	 Just x -> error $ show id ++ " already bound to " ++ show x
	 Nothing -> addListToFM env [(id, exp)]

act :: Exp -> Env -> IO Env
act inp @ (App fun [ App id [], right ]) env | fun == assign =
    do
	putStrLn $ show inp
	let val = value env right
	putStrLn $ "  =  "  ++ show val
	let env' = update env id val
	return env'

act x env = error $ "act: " ++ show x


acts :: [ String ] -> IO Env
acts xs = pacts (parsed $ unlines xs) env0

pacts [] env = return env
pacts (x : xs) env = 
    do 
       env' <- act x env
       pacts xs env'


----------------------------------------------------------------

env0 :: Env
env0 = listToFM [ (usercon 0 ("H" ++ show k), x) 
		| (k, x) <- zip [1..] axioms
		]
