{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module NFA.Gen where

import NFA.Genetic
import NFA.Synchronize

import Autolib.NFA.Dot
import Autolib.NFA.Type
import Autolib.Util.Zufall
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Output

import Autolib.Sets
import Autolib.Schichten

import Random
import Data.List
import Control.Monad
import Text.Html (Html)

type Gene = [(Int,Int)]



conf :: Int -> Conf Gene Int
conf n = Conf 
       { fitness = \ g ->  case shosyn $ mach g of
	                     w : _ | keinkreis g -> length w
	                     _     -> 0
       , threshold = (n-1) ^ 2
       , generate = sequence $ replicate n $ do
            let qs = [0 .. n-1]
	    x <- eins qs ; y <- eins qs
            return (x, y)
       , combine = \ a b -> do
            return $ zip ( map fst a ) ( map snd b )
       , mutate = often 2 $ \ a -> entweders
            [ -- flip two letters (from one state)
              do i <- randomRIO (0, n-1)
	         return $ update a (i, \ (x,y) -> (y,x) )
              -- change direction of one arrow
	    , do i <- randomRIO (0, n-1)
	         f <- randomRIO (False, True)
                 z <- randomRIO (0, n-1)
                 return $ update a (i, \ (x,y) -> if f then (z,y) else (x,z))
              -- swap two states
            , do [i, j] <- einige 2 [0 .. n-1]
	         return $ pokes a [(i, a!!j), (j, a!!i)]
	    ]
       , size = 200
       , num_mutate = 300
       , num_combine = 400
       , trace = \ pool -> print $ toDoc $ take 3 $ popul pool
       , present = \ pool -> schreib n pool
       }

schreib :: Int
	-> Pool Gene Int
	-> IO ()
schreib n pool = tofile ("pool-" ++ show n ++ "-" ++ show (num pool)) $ do
    inform $ fsep [ text "generation", toDoc $ num pool ]
    sequence_ $ do
        (v, g) <- take 3 $ popul pool
        return $ handle $ mach g

tofile :: FilePath 
     -> Reporter a
     -> IO ()
tofile fname r = do
        ( _ , out :: Text.Html.Html ) <- Autolib.Reporter.run r
	writeFile ( fname ++ ".html" ) $ show out


keinkreis :: Gene -> Bool
keinkreis g =
    let orbit xs = bfs ( \ k -> unitSet (xs !! k) ) ( head xs )
        full orb = length orb == length g
    in  and $ do 
           f <- [ fst, snd ]
           return $ not $ full $ orbit $ map f g

mach g = make $ umform g 
umform g = [map fst g, map snd g]

often :: Monad m 
      => Int 
      -> (a -> m a)
      -> (a -> m a)
often k this = \ a -> foldM ( \ a () -> this a ) a ( replicate k () )

update :: [a] -> (Int, a -> a) -> [a]
update xs (i, f) = poke xs (i, f $ xs !! i)

poke :: [a] -> (Int, a) -> [a]
poke xs (i, y) = 
    let (pre, _ : post) = splitAt i xs
    in  pre ++ [y] ++ post

pokes :: [a] -> [(Int, a)] -> [a]
pokes = foldl poke