module Resolution.Roll where

import Resolution.Data
import Resolution.Config

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.TES.Identifier
import Autolib.Set

import OBDD ( OBDD )
import qualified OBDD as O

variables :: Int -> [ Identifier ]
variables n = take n $ map ( mkunary . return ) [ 'a' .. ]

rclause :: Config -> IO Clause
rclause conf = do 
    s <- randomRIO $ literals_per_clause_bounds conf
    vs <- selektion s $ variables $ num_variables conf
    ls <- sequence $ do 
        v <- vs 
        return $ do 
            p <- randomRIO ( False, True )
            return $ Literal { name = v, parity = p }
    return $ Clause $ mkSet ls

semantics :: Clause -> OBDD Identifier
semantics ( Clause xs ) = 
    foldr ( O.|| ) ( O.constant False ) $ do
        x <- setToList xs
        return $ O.unit ( name x ) ( parity x )

rset :: Config -> IO ( [ Clause ], Clause )
rset conf = do
    t <- case target conf of
           Empty -> return $ Clause emptySet
           Random -> rclause conf
    let extend current cs = 
            if O.null current then return cs else do
                  c <- rclause conf
                  if c `elem` cs 
                      then extend current cs
                      else extend ( current O.&&  semantics c ) ( c : cs )
    cs <- extend ( O.not $ semantics t ) []
    return ( cs, t )

medium :: Config -> IO ( [ Clause ], Clause )
medium conf = do
    let n = 5
    candidates  <- sequence $ replicate n $ rset conf
    return $ sortBy ( \ (cs, t) -> length cs ) candidates !! (n `div` 2)

