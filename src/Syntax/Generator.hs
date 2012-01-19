{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module Syntax.Generator where

import Syntax.Syntax
import Syntax.Words

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
--import System.Random
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Random

import Data.List
import Data.Generics


terminals = everything (++) ([] `mkQ` q)
 where
   q (Terminal t) = [t]
   q _            = []

symbols = everything (++) ([] `mkQ` q)
 where
   q (Symbol s) = [s]
   q _          = []


instance (MonadPlus m, RandomGen g) => MonadPlus (RandT g m) where
    mzero = lift mzero
    mplus x y = getSplit >>= \g -> lift (evalRandT x g `mplus` evalRandT x g)

instance MonadRandom m => MonadRandom (MaybeT m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR r = lift (getRandomR r)
    getRandomRs r = lift (getRandomRs r)


--generate :: IO [(String,Graph)]
generate = do
   l <- getRandomLang
   --putStrLn "===================="
   --putStrLn $ intercalate "\n" $ concatMap (ascii . snd) l
   if isNice l
      then return l
      else generate

getRandomLang = runMaybeT lang >>= maybe getRandomLang return

isNice l =
   ["a","b","c","d"] `subset` terminals l
   &&
   let smallestWords = take 4 $ generateWords 7 l in
   4 == length smallestWords
   &&
   let nonterminalsInvolved = nub $ filter (`elem`["A","B","C","D"]) $ concat $ map snd smallestWords in
   3 == length (take 3 nonterminalsInvolved)
   &&
   any ((5<=).length.filter (`elem`["A","B","C","D"]).snd) smallestWords


subset xs ys = all (`elem` ys) xs


--instance MonadPlus (RandT StdGen Maybe) where
--   m1 `mplus` m2 = runRandT

-- >>= \g -> guard () >> return g


lang = sequence
         [ ("A",) <$> evalStateT graph 6
         , ("B",) <$> evalStateT graph 4
         , ("C",) <$> evalStateT graph 4
         , ("D",) <$> evalStateT graph 4
         ]


mustContain xs g = do
    guard (all (`elem`symbols g) xs)
    return g

graph = do
   n <- get
   guard (n > 0)
   frequency
         [ (50, Chain <$> graph' <*> graph')
         , (10, Fork  <$> graph' <*> graph')
         , (10, Fork  <$> graph' <*> return Empty)
         , ( 5, Loop  <$> graph')
         , (15, Terminal <$> terminal)
         , (15, Symbol   <$> symbol)
         ]
      where graph' = (modify pred) >> graph

terminal = oneof $ map return ["a","b","c","d"]
symbol = oneof $ map return ["A","B","C","D"]

oneof [] = fail "oneof used with empty list"
oneof gs = do
   i <- choose (0,length gs - 1)
   (gs !! i) <|> (oneof (gs \! i))

-- | Chooses one of the given generators, with a weighted random distribution.
-- The input list must be non-empty.
--frequency :: [(Int, Gen a)] -> Gen a
frequency [] = fail "frequency used with empty list"
frequency xs0 = choose (1 :: Int, tot) >>= (\n -> pick n xs0 0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs) i
    | n <= k    = x <|> frequency (xs0 \! i)
    | otherwise = pick (n-k) xs (succ n)
  pick _ _  _ = fail "pick used with empty list"

(\!) :: [a] -> Int -> [a]
xs \! i = take i xs ++ drop (succ i) xs

choose = getRandomR

