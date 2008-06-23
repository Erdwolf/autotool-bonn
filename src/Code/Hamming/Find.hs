module Code.Hamming.Find where

import Code.Hamming.Data
import Code.Hamming.Partial
import Code.Hamming.Check ( dist )

import Challenger.Partial
import Autolib.Reporter
import Autolib.Util.Zufall

import qualified Autolib.Genetic as G

import Data.Maybe ( fromMaybe )
import Data.List ( tails )

main = G.evolve 
     $ make
     $ Config { width = ( Fixed , 7  )
            , size   = ( Fixed, 17 )
            , distance = ( Atleast, 0 )
            , optimize = Distance
            }


make :: Config -> G.Config [LR] ( Integer, Double )
make conf =  G.Config 
       { G.fitness = \ g -> fromMaybe ( 0, 0) $ result $ do
                 let code = geno2pheno conf g
                 partial Hamming conf code
                 total Hamming conf code
                 return ( measure Hamming conf code
                        , total_distance code
                        )
          , G.threshold = ( 1000, 0 )
          , G.present = \ vas -> print $ take 3 $ vas
          , G.trace   = \ vas -> print $ take 3 $ vas
          , G.size    = 100
          , G.generate = do
                let ( Fixed, s ) = size conf
                    ( Fixed, w ) = width conf
                sequence $ replicate ( s * w ) $ eins [ L, R ]
          , G.combine = \ xs ys -> do
                i <- randomRIO ( 0, length xs )
                let xs' = take i xs ; ys' = drop i ys
                return $ xs' ++ ys'
          , G.num_combine = 50
          , G.mutate  = \ xs -> sequence $ do
                x <- xs
                return $ do
                    i <- randomRIO ( 0, length xs )
                    if i > 2 then return x else eins [ L, R ]
          , G.num_mutate = 50
          , G.num_compact = 30
          , G.num_steps = Nothing
          , G.num_parallel = 1
          }

total_distance ws = sum $ do
    u : vs <- tails ws
    v <- vs
    return $ ( fromIntegral $ dist u v ) ** 0.1

geno2pheno :: Config -> [ LR ] ->  [[LR]]
geno2pheno conf xs = 
    let ( Fixed , w ) = width conf
    in  cuts w xs

cuts w xs = 
    let ( pre, post ) = splitAt w xs
    in  if null pre then [] else pre : cuts w post

