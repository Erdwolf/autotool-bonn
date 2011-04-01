module Petri.Roll where

import Petri.Type
import Autolib.Util.Zufall
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( forM ) 

net ::  ( Ord s , Ord t )
    => [s] -> [t] -> Capacity s
    -> IO ( Net s t )
net ps ts cap = do        
    s <- state ps
    cs <- conn ps ts
    return $ Net
        { places = S.fromList ps
        , transitions = S.fromList ts                    
        , connections = cs                
        , capacity = cap
        , start = s
        }
    
state ps = do
    qs <- selection ps
    return $ State $ M.fromList $ do
        p <- ps
        return ( p, if p `elem` qs then 1 else 0 )

conn :: ( Ord s , Ord t )
    => [s] -> [t]
    -> IO [ Connection s t ]
conn ps ts = forM ts $ \ t -> do
    vor <- selection ps    
    nach <- selection ps
    return ( vor, t, nach )
    
-- | pick a non-empty subset,    
-- size s with probability 2^-s
selection :: [a] -> IO [a]
selection [] = return []
selection xs = do
    i <- randomRIO ( 0, length xs - 1 )
    let ( pre, x : post ) = splitAt i xs
    f <- randomRIO ( False, True )
    xs <- if f then selection $ pre ++ post else return []
    return $ x : xs
  
