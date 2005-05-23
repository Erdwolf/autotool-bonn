module RM.Step where

import RM.Type
import RM.Memory
import RM.State

import Control.Monad ( guard )

step :: State -> [ State ]
step s = do

    guard $ program s /= Nothing

    let (Just p) = program s
    let m        = memory s
    let stop     = s { schritt = succ $ schritt s
		     , past    = s : past s
		     , program = Nothing 
		     }

    case p of Add n       -> return $ stop { memory = inc m n }
              Sub n       -> return $ stop { memory = dec m n }

	      Conc [x]    -> step $ s { program = Just x }
	      Conc (x:xs) -> do 
	                q <- step $ s { program = Just x }
			return $ case program q of
			  Nothing -> q { program = Just $ Conc     xs  }
			  Just x' -> q { program = Just $ Conc (x':xs) }
              Conc _      -> return stop

	      While n x   -> case get m n of
                        0 -> return stop
			_ -> do
                        q <- step $ s { program = Just x }
			return $ case program q of
			  Nothing -> q { program = Just p }
			  Just x' -> q { program = Just $ Conc [x',p] }
