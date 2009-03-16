-- module Main where



import Strings.Reverse.Data
import Strings.Reverse.Operator

import  Autolib.Genetic 
import  Autolib.ToDoc ( ToDoc )
import  qualified Autolib.Size
import  Autolib.Util.Zufall

import System.Environment
import System.IO
import Data.List ( nub )

main = do
    [ u, v ] <- getArgs
    let sigma = nub $ u ++ v
    evolve $ config sigma u v

base = 100
tour = 2

distance ::  Eq a => [a] -> [a] -> Int
distance u v = 
    let length_diff = abs ( length u - length v )
        contents_diff = 
           sum $ zipWith ( \ x y -> if x /= y then 1 else 0 ) u v 
           -- length $ dropWhile id $ zipWith (==) u v
    in  length_diff + contents_diff

config :: ( ToDoc a, Ord a ) 
       => [a] -> [a] -> [a] -> Config [Int] Double
config items u v = 
     Config { fitness = \ ps ->
                  let x = make items ps
                      p = original x
                      q = semantics x
                  in  negate ( fromIntegral $ distance u p +  distance v q )
                      -- + 1 / (1 + fromIntegral ( length ps ) )
                      -- + 1 / (1 + fromIntegral ( Autolib.Size.size x ) )
          , threshold = 0
          , present = \ kvs -> sequence_ $ do
                        (k,v) <- take 3 kvs
                        return $ printf ( k, make items v )
          , trace   = \ kvs -> return () -- mapM_ printf $ take 3 kvs
          , size    = base
          , generate = sequence $ replicate ( length $ u ++ v ) 
                                $ randomRIO ( 0, length $ u ++ v )
          , combine = \ xs ys -> do
                com <- eins [ onepoint, twopoint, zipper ]
                com xs ys
          , num_combine = base
          , mutate  = \ xs -> do
                mut <- eins [ shift, double, turn, uniform ]
                mut xs
          , num_mutate = tour
          , num_steps = Nothing
          , num_parallel = 1
          , scheme = Tournament tour
          }

printf x = do print x ; hFlush stdout
